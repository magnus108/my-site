--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import           Data.Maybe

import Debug.Trace
-------------------------------------------------------------------------------
content = "**.markdown"


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match content $
        version "routes" $ do
            compile $ do
                underLying <- fmap (setVersion Nothing) getUnderlying
                route <- getRoute underLying
                makeItem (fromMaybe "" route)


--    match (fromList ["about.rst", "contact.markdown", "404.markdown"]) $ do
  {-
    match content $ do
        route   $ setExtension "html"
        compile $ do
            routes <- loadAll (hasVersion "routes")

            currentRoute <- getRoute =<< getUnderlying

            let menuCtx = listField "menu"
                              (   field "url" urlFromFilePathItem
                              <>  boolField "currentRoute"
                                      (isItem (fromMaybe "" currentRoute))
                              <>  defaultContext
                              )
                              (return routes)
                          <> defaultContext

            pandocCompiler
                >>= loadAndApplyTemplate "templates/menu.html"    menuCtx
                >>= loadAndApplyTemplate "templates/default.html" menuCtx
                >>= relativizeUrls
                -}

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-------------------------------------------------------------------------------
urlFromFilePathItem :: Item String -> Compiler String
urlFromFilePathItem item = return (toUrl (itemBody item))


isItem :: FilePath -> Item String -> Bool
isItem x item = itemBody item == x
