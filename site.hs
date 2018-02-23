--------------------------------------------------------------------------------
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow (first, (&&&))
import           Control.Monad (liftM, zipWithM_)
import           Control.Applicative (empty, (<$>))
import           Data.Monoid (mempty, mconcat, mappend, (<>))
import           Hakyll
import           Data.Maybe
import           System.FilePath
import           System.FilePath.Posix (takeBaseName,takeDirectory,(</>))
import           Data.List (insert, partition, delete, isSuffixOf, dropWhileEnd)
import           Data.List.Extra (takeWhileEnd)
import           Data.List.Split


import qualified Data.Char as C (toLower)


import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

import Debug.Trace
-------------------------------------------------------------------------------
posts = "posts/*/*/*.markdown"
employees = "employees/*/about.markdown"

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
        addToCountry
        route cleanRoute2
        compile $ do
            menu <- getMenu

            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" (menu <> defaultContext)
                >>= relativizeUrls
                >>= cleanIndexUrls


    match employees $ do
        addToMenu
        addToEmployee
        route cleanRoute2
        compile $ do
            menu <- getMenu

            pandocCompiler
                >>= loadAndApplyTemplate "templates/employee.html" (menu <> countriesCtx <> defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" (menu <> countriesCtx <> defaultContext)
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
                >>= loadAndApplyTemplate "templates/post.html" (employeeCtx <> postCtxWithTags tags <> postImageField <> menu <> defaultContext)
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


    --- hvorfor er du forsvundet fra menuen?g
    match "index.html" $ do
        addToMenu
        route idRoute
        compile $ do
            menu <- getMenu

            --posts skal måske også hives ud?
            posts <- recentFirst =<< loadAll (posts .&&. hasNoVersion)


            employees <- loadAll (employees .&&. hasNoVersion)

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "employees" defaultContext (return employees) `mappend`
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
--collectTags tags = filter (\x -> (itemBody x) == "bar1") $ map (\(t, _) -> Item (tagsMakeId tags t) t) (tagsMap tags)



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
addToEmployee :: Rules ()
addToEmployee = version "employees" $ compile $ makeItem =<< toEmployee


toEmployee :: Compiler String
-- boi this uggly
toEmployee = (\y -> fmap (\x -> getLastFolder x ++ ":" ++ (toLower y)) getResourceFilePath) =<< metaCountries


metaCountries :: Compiler String
metaCountries = getUnderlying >>= (\i -> getMetadataField' i "countries")
-------------------------------------------------------------------------------
addToCountry :: Rules ()
addToCountry = version "countries" $ compile $ makeItem =<< toCountry


toCountry :: Compiler String
toCountry = fmap getLastFolder getResourceFilePath

getLastFolder :: FilePath -> String
getLastFolder f = last (splitDirectories (takeDirectory f))

-------------------------------------------------------------------------------
addToMenu :: Rules ()
addToMenu = version "routes" $ compile $ makeItem =<< maybeToRoute Nothing


maybeToRoute :: Maybe String -> Compiler String
maybeToRoute v = fmap (fromMaybe "") (routeForUnderlying v)


routeForUnderlying :: Maybe String -> Compiler (Maybe FilePath)
routeForUnderlying v = getRoute =<< (setUnderlyingVersion v)


setUnderlyingVersion :: Maybe String -> Compiler Identifier
setUnderlyingVersion v = fmap (setVersion v) getUnderlying


-------------------------------------------------------------------------------
employeeCtx :: Context String
employeeCtx =
    field "employee" getEmployee


--okay først få den til at get employee senere check den employee faktisk sælger land 
-- dette kan lade sig gøre ved at tilføje mere til addToEmployee..
getEmployee :: Item a -> Compiler String
getEmployee item = do
    country <- getResourceFilePath >>= (\f -> return $ splitDirectories (takeDirectory f) !! 2)
    employee <- getMetadataField' (itemIdentifier item) "employee"
    employees <- loadAll (hasVersion "employees")
    if any (employeeSellsCountry employee country . itemBody) employees then
        return employee
    else
        error $ "could not parse employee field:" ++ employee


employeeSellsCountry :: String -> String -> String -> Bool
employeeSellsCountry currentEmployee currentCountry body =
    toLower currentEmployee == employee && currentCountry `elem` (splitOn "," countries)
        where
            employee = takeWhile ((/=) ':') body
            countries = takeWhileEnd ((/=) ':') body

-------------------------------------------------------------------------------

countryCtx :: Context String
countryCtx = field "title" (\item -> return $ itemBody item)


countriesCtx :: Context String
countriesCtx =
    listFieldWith "countries" countryCtx getCountries <>
    defaultContext


getCountries :: Item a -> Compiler [Item String]
getCountries item = do
    countries <- getMetadataField' (itemIdentifier item) "countries"
    mapM getCountry (splitOn "," countries)


getCountry :: String -> Compiler (Item String)
getCountry x = do
    countries <- loadAll (hasVersion "countries")
    -- okay fst af itemBody er italy... snd er Italy? 
    -- okay fst af itemBody er italy... snd er Italy? 
    --  MAYBEINSTEADER JUST DEKAPITALIZZE??!? altså lav Italy til italy?
    if toLower x `elem` (fmap (itemBody) countries) then
        makeItem x
    else
        error $ "could not parse countries field:" ++ x

toLower = fmap C.toLower

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


data MenuLevel = MenuLevel { prevItems :: [FilePath]
                           , currItem  :: FilePath
                           , aftItems  :: [FilePath]
                           } deriving (Show)

allItems :: MenuLevel -> [FilePath]
allItems l = prevItems l ++ aftItems l


initMenuLevel :: FilePath -> MenuLevel
initMenuLevel x = MenuLevel [] x []


insertUniq :: Ord a => a -> [a] -> [a]
insertUniq x xs | x `elem` xs = xs
                | otherwise = insert x xs


insertItem :: MenuLevel -> FilePath -> MenuLevel
insertItem l v = case aftItems l of
                    []     -> atPrev
                    (x:xs) | v < x     -> atPrev
                           | otherwise -> l { aftItems = x:insertUniq v xs }
    where atPrev = l { prevItems = insertUniq v (prevItems l) }


insertFocused :: MenuLevel -> FilePath -> MenuLevel
--insertFocused l v = MenuLevel bef (v:aft)
 --   where (bef, aft) = partition (<v) (delete v $ allItems l)
insertFocused l v = MenuLevel bef v aft
    where (bef, aft) = partition (<v) (allItems l)



newtype MMenu a b = MMenu { menuLevels :: a b } deriving (Show, Foldable)

type Menu = MMenu [] MenuLevel

emptyMenu :: Menu
emptyMenu = MMenu []


relevant :: FilePath -> FilePath -> [FilePath]
relevant this other = relevant' (splitPath this) (splitPath other)
   where relevant' (x:xs) (y:ys) = y : if x == y then relevant' xs ys else []
         relevant' [] (y:_) = [y]
         relevant' _ _ = []


buildMenu :: FilePath -> [FilePath] -> Menu
buildMenu currentRoute routes = foldl (extendMenu currentRoute) emptyMenu routes


extendMenu :: FilePath -> Menu -> FilePath -> Menu
extendMenu currentRoute menu route =
    foldl add menu (relevant currentRoute route)
        where
            add acc y = acc

--extendMenu currentRoute m path =
  {-Menu $
    add (menuLevels m) (relevant this' path') "/"
    where add ls [] _ = ls
          add ls (x:xs) p
            | x `elem` focused = insertFocused l (p++x) : add ls' xs (p++x)
            | otherwise        = insertItem l (p++x) : add ls' xs (p++x)
            where (l,ls') = case ls of []  -> (initMenuLevel path', [])
                                       k:ks -> (k,ks)
          focused = splitPath this'
          path' = normalise path
          this' = normalise currentRoute
          -}


showMenu :: Menu -> H.Html
showMenu = zipWithM_ showMenuLevel [0..] . menuLevels


showMenuLevel :: Int -> MenuLevel -> H.Html
showMenuLevel d m =
  H.ul (mapM_ H.li elems) ! A.class_ (H.toValue $ "menu" ++ show d)
    where showElem (p) = H.a (H.toHtml p) ! A.href (H.toValue p)
          showFocusElem (p) = showElem (p) ! A.class_ "thisPage"
          elems = map showElem (prevItems m) ++ [showFocusElem (currItem m)] ++ map showElem (aftItems m)
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


-- add sælger to travel.. husk du kan lave ligsom med add to menu og så filtrer. du kan sikkert også kaste en fejl
-- Du skal fixe menuen.. overvej om rejserne skal tilføjes til menuen?
-- OVERVEJ OM DU BARE  BØR LAVE EN MASSE submenuer.. det er nemmere og mere flexibelt?

            -- UPDATE THIS LIST DYNAMICALLY USING A function like addToMenu but addToCountries
            -- YOU can also do this for each travel!
            --let isCountry x = if x `elem` ["China", "Africa", "Italy", "Thailand"] then
