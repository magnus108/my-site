--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow (first, (&&&))
import           Control.Monad (liftM, zipWithM_)
import           Control.Applicative (empty, (<$>))
import           Data.Monoid (mempty, mconcat, mappend, (<>))
import           Hakyll
import           Data.Maybe
import           System.FilePath
import           System.FilePath.Posix (takeBaseName,takeDirectory,(</>))
import           Data.List (insert, partition, delete, isSuffixOf)


import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

import Debug.Trace
-------------------------------------------------------------------------------
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

    match "posts/*/about.markdown" $ do
        addToMenu
        route cleanRoute2
        compile $ do
            menu <- getMenu

            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" (menu <> defaultContext)
                >>= relativizeUrls
                >>= cleanIndexUrls


    match (fromList ["contact.markdown", "404.markdown"]) $ do
        addToMenu
        route cleanRoute
        compile $ do
            menu <- getMenu

            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" (menu <> defaultContext)
                >>= relativizeUrls
                >>= cleanIndexUrls


    pag <- buildPaginateWith grouper posts makeId


    paginateRules pag $ \pageNum pattern -> do
        addToMenu
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern

            menu <- getMenu

            let paginateCtx = paginateContext pag pageNum
                ctx =
                    constField "title" ("Blog Archive - Page " ++ (show pageNum)) <>
                    listField "posts" defaultContext (return posts) <>
                    paginateCtx <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blogpage.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" (ctx <> menu)
                >>= relativizeUrls
                >>= cleanIndexUrls


    tags <- buildTags posts (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route cleanRoute
        compile $ do
            menu <- getMenu

            posts <- recentFirst =<< loadAll pattern

            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" (ctx <> menu)
                >>= relativizeUrls
                >>= cleanIndexUrls


    match posts $ do
        addToMenu
        route $ cleanRouteWithoutDate
        compile $ do
            menu <- getMenu

            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags <> postImageField <> menu <> defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags <> postImageField <> menu <> defaultContext)
                >>= relativizeUrls
                >>= cleanIndexUrls


    match "archive.html" $ do
        addToMenu
        route cleanRoute
        compile $ do
            menu <- getMenu

            posts <- recentFirst =<< loadAll (posts .&&. hasNoVersion)

            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" (archiveCtx <> menu)
                >>= relativizeUrls
                >>= cleanIndexUrls


    match "index.html" $ do
        addToMenu
        route idRoute
        compile $ do
            menu <- getMenu

            --posts skal måske også hives ud?
            posts <- recentFirst =<< loadAll (posts .&&. hasNoVersion)

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "tags" defaultContext (return (collectTags tags)) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (indexCtx <> menu)
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


---LOL
cleanRoute2 :: Routes
cleanRoute2 = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> "index.html"
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


-------------------------------------------------------------------------------
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery 2) . sortRecentFirst) ids

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "blog/page/" ++ (show pageNum) ++ ".html"
-------------------------------------------------------------------------------


addToMenu :: Rules ()
addToMenu = version "routes" $ compile $ makeItem =<< maybeToRoute Nothing


maybeToRoute :: Maybe String -> Compiler String
maybeToRoute v = fmap (fromMaybe "") (routeForUnderlying v)


routeForUnderlying :: Maybe String -> Compiler (Maybe FilePath)
routeForUnderlying v = getRoute =<< (setUnderlyingVersion v)


setUnderlyingVersion :: Maybe String -> Compiler Identifier
setUnderlyingVersion v = fmap (setVersion v) getUnderlying


getMenu :: Compiler (Context String)
getMenu = do
  routes <- moveIndexToFront =<< loadAll (hasVersion "routes")
  currentRoute <- maybeToRoute Nothing
  --return (listField "menu" (menuCtx currentRoute) (return routes))
--  lol <- (return $ buildMenu currentRoute (fmap itemBody routes))

--  traceM $ show lol
--  traceM $ show currentRoute
--  traceM $ show routes
  return $ constField "menu" $ renderHtml $ showMenu $ buildMenu currentRoute (fmap itemBody routes)

-- buildMenu currenRotue routes 
-- currentRoute er vigigt for relativiteten..
-- i min templtefil skal jeg have menuLevel...

menuCtx :: FilePath -> Context String
menuCtx currentRoute =
    field "url" urlFromFilePathItem <>
    boolField "currentRoute" (isItem currentRoute) <>
    defaultContext


-------------------------------------------------------------------------------


data MenuLevel = MenuLevel { prevItems :: [(FilePath,String)]
                           , aftItems  :: [(FilePath,String)]
                           } deriving (Show)

allItems :: MenuLevel -> [(FilePath, String)]
allItems l = prevItems l ++ aftItems l


emptyMenuLevel :: MenuLevel
emptyMenuLevel = MenuLevel [] []


insertUniq :: Ord a => a -> [a] -> [a]
insertUniq x xs | x `elem` xs = xs
                | otherwise = insert x xs


insertItem :: MenuLevel -> (FilePath, String) -> MenuLevel
insertItem l v = case aftItems l of
                    []     -> atPrev
                    (x:xs) | v < x     -> atPrev
                           | otherwise -> l { aftItems = x:insertUniq v xs }
    where atPrev = l { prevItems = insertUniq v (prevItems l) }


insertFocused :: MenuLevel -> (FilePath, String) -> MenuLevel
insertFocused l v = MenuLevel bef (v:aft)
    where (bef, aft) = partition (<v) (delete v $ allItems l)


newtype Menu = Menu { menuLevels :: [MenuLevel] } deriving (Show)

emptyMenu :: Menu
emptyMenu = Menu []


relevant :: FilePath -> FilePath -> [FilePath]
relevant this other = relevant' (splitPath this) (splitPath other)
   where relevant' (x:xs) (y:ys) = y : if x == y then relevant' xs ys else []
         relevant' [] (y:_) = [y]
         relevant' _ _ = []


buildMenu :: FilePath -> [FilePath] -> Menu
buildMenu this = foldl (extendMenu this) emptyMenu
                 . map (first dropIndex . (id &&& dropExtension . takeFileName))

dropIndex :: FilePath -> FilePath
dropIndex p | takeBaseName p == "index" = dropFileName p
            | otherwise                 = p

extendMenu :: FilePath -> Menu -> (FilePath, String) -> Menu
extendMenu this m (path, name) =
    if path' `elem` ["./", "/", ""] then m else
       Menu $ add (menuLevels m) (relevant this' path') "/"
    where add ls [] _ = ls
          add ls (x:xs) p
            | x `elem` focused = insertFocused l (p++x,name') : add ls' xs (p++x)
            | otherwise        = insertItem l (p++x,name') : add ls' xs (p++x)
            where (l,ls') = case ls of []  -> (emptyMenuLevel, [])
                                       k:ks -> (k,ks)
                  name' = if hasTrailingPathSeparator x then x else name
          focused = splitPath this'
          path' = normalise path
          this' = normalise this




showMenu :: Menu -> H.Html
showMenu = zipWithM_ showMenuLevel [0..] . menuLevels

showMenuLevel :: Int -> MenuLevel -> H.Html
showMenuLevel d m =
  H.ul (mapM_ H.li elems) ! A.class_ (H.toValue $ "menu" ++ show d)
    where showElem (p,k) = H.a (H.toHtml k) ! A.href (H.toValue p)
          showFocusElem (p,k) = showElem (p,k) ! A.class_ "thisPage"
          elems = map showElem (prevItems m) ++
                  case aftItems m of []     -> []
                                     (l:ls) -> showFocusElem l :
                                               map showElem ls
{-
<ul>
    $for(menu)$
        <li>
            <a href="$url$">
                $if(currentRoute)$
                    <strong>$title$</strong>
                $else$
                    $title$
                $endif$
            </a>
        </li>
    $endfor$
</ul>
-}

-- Du skal fixe menuen.. overvej om rejserne skal tilføjes til menuen?
