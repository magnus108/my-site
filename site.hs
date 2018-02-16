--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (empty, (<$>))
import           Data.Monoid (mempty, mconcat, mappend, (<>))
import           Hakyll
import           Data.Maybe
import           System.FilePath
import           System.FilePath.Posix (takeBaseName,takeDirectory,(</>))
import           Data.List (isSuffixOf)

import Debug.Trace
-------------------------------------------------------------------------------
content = "**.markdown"
posts = "posts/*/*/*.markdown"


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    let (postImages, postImageField) = imageProcessor "posts/*/*/banner.png"
                                         [ ("small" , Just (200,100))
                                         , ("medium", Just (600,300))
                                         , ("full"  , Nothing)
                                         ]

    postImages

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (content .||. "index.html") $
        version "routes" $ do
            compile $ do
                underLying <- fmap (setVersion Nothing) getUnderlying
                route <- getRoute underLying
                makeItem (fromMaybe "" route)


--    match (fromList ["about.rst", "contact.markdown", "404.markdown"]) $ do

    match (fromList ["contact.markdown", "404.markdown"]) $ do
        route cleanRoute
        compile $ do
            routes <- moveIndexToFront =<< loadAll (hasVersion "routes")

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
                >>= loadAndApplyTemplate "templates/default.html" menuCtx
                >>= relativizeUrls
                >>= cleanIndexUrls


    tags <- buildTags posts (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
--                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls


    match posts $ do
        route $ cleanRouteWithoutDate
        compile $ do

            routes <- moveIndexToFront =<< loadAll (hasVersion "routes")

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
                >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags <> postImageField <> menuCtx)
                >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags <> postImageField <> menuCtx)
                >>= relativizeUrls
                >>= cleanIndexUrls


{-    create ["archive.html"] $ do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll (posts .&&. hasNoVersion)
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls
-}

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (posts .&&. hasNoVersion)
            routes <- moveIndexToFront =<< loadAll (hasVersion "routes")

            currentRoute <- getRoute =<< getUnderlying

            let menuCtx = listField "menu"
                              (   field "url" urlFromFilePathItem
                              <>  boolField "currentRoute"
                                      (isItem (fromMaybe "" currentRoute))
                              <>  defaultContext
                              )
                              (return routes)
                          <> defaultContext

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "tags" defaultContext (return (collectTags tags)) <>
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (indexCtx <> menuCtx)
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
-------------------------------------------------------------------------------
urlFromFilePathItem :: Item String -> Compiler String
urlFromFilePathItem item = return (toUrl (itemBody item))


isItem :: FilePath -> Item String -> Bool
isItem x item = itemBody item == x


collectTags tags = map (\(t, _) -> Item (tagsMakeId tags t) t) (tagsMap tags)


cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident


cleanRouteWithoutDate :: Routes
cleanRouteWithoutDate = customRoute createIndexRoute
  where
    createIndexRoute ident = directory </> (drop 11 (takeBaseName p)) </> "index.html"
                            where 
                                p = toFilePath ident
                                directory =
                                    joinPath(init(splitDirectories (takeDirectory p)))

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)


cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"


cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"


--------------------------------------------------------------------------------
-- Image processing
--------------------------------------------------------------------------------

type ImageProcessing = [(String, Maybe (Int, Int))]

-- | Process image files according to a specification.
--
-- The 'Rules' and 'Context'  returned can be used to output and
imageProcessor :: Pattern -- ^ Images to process.
               -> ImageProcessing -- ^ Processing instructions.
               -> (Rules (), Context a)
imageProcessor pat procs = let field = imageField pat procs
                               rules = imageRules pat procs
                            in (rules, field)

-- | Generate 'Rules' to process images.
imageRules :: Pattern -- ^ Pattern to identify images.
           -> ImageProcessing -- ^ Versions to generate.
           -> Rules ()
imageRules pat procs = match pat $ do
  sequence_ $ map processImage procs
  where
    imageRoute name ident = let path = toFilePath ident
                                base = takeFileName path
                                name' = name ++ "-" ++ base
                            in replaceFileName path name'
    -- Process an image with no instructions.
    processImage (name, Nothing) = version name $ do
        route $ customRoute (imageRoute name)
        compile $ copyFileCompiler
    -- Process with scale and crop instructions.
    processImage (name, Just (x,y)) = version name $ do
        route $ customRoute (imageRoute name)
        let cmd = "convert"
        let args = [ "-"
                   , "-resize"
                   , concat [show x, "x", show y, "^"]
                   , "-gravity"
                   , "Center"
                   , "-crop"
                   , concat [show x, "x", show y, "+0+0"]
                   , "+repage"
                   , "-"
                   ]
        compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)

-- | Add image versions associated with an 'Item' to the context.
--
-- Variables defined
imageField :: Pattern -- ^ Pattern to identify images.
           -> ImageProcessing -- ^ Versions to generate.
           -> Context a
imageField pat procs = mconcat $ map (fff pat) procs
  where
    idPath = toFilePath . flip fromCaptures (map show [1..])
    fff p (name, _) = let imgpath = idPath p
                          imgfile = takeFileName imgpath
                          key = (takeBaseName imgpath) ++ "-" ++ name
                      in field key $ \item ->
                          let path = toFilePath $ itemIdentifier item
                              (dir, file) = splitFileName path
                              path' = combine dir imgfile
                              imgid = setVersion (Just name) $ fromFilePath path' 
                          in do
                            mroute <- getRoute imgid
                            case mroute of
                              Nothing -> empty
                              Just route -> return $ "<img src='" ++ (toUrl route) ++ "'>"





-------------------------------------------------------------------------------

moveIndexToFront :: MonadMetadata m => [Item String] -> m [Item String]
moveIndexToFront itemList =
    return (moveToFront "index.html" itemList)
        where
          moveToFront x xs =
            case break (\y -> itemBody y == x) xs of
              (a,y:ys) -> y:a ++ ys
              (a,ys) -> a ++ ys

 --   sortByM $ getItemUTC defaultTimeLocale . itemIdentifier
 -- where
  --  sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
   -- sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
    --               mapM (\x -> liftM (x,) (f x)) xs


-- fix menu like troels.. we have to define content and non-content.. overvej hvad kan hives ud af menu functionen.
-- overvej multi-tag site altså tag africa og thailand
-- overvej ikke så meget folder structur.. men du skal kunne have lande billede jo... det er et spørgsmål om hvordan jeg vil parse
