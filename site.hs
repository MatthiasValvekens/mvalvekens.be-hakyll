--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}


import Hakyll

import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkPandocM, walk)
import Text.Pandoc 
    ( Extension (..)
    , ReaderOptions (..)
    , WriterOptions (..)
    , HTMLMathMethod (..)
    , enableExtension )
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Citeproc as Pandoc (processCitations)
import Hakyll.Core.Compiler.Internal (compilerThrow)

import Data.Ord (comparing)
import Data.List (sortBy, nub, intercalate, find)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Data.ByteString.Lazy (toStrict)

import Control.Monad (liftM, liftM2, (>=>), msum)
import System.FilePath (takeBaseName)

import qualified GHC.IO.Encoding as E

import qualified Data.Aeson as Aes
import Data.Scientific (toBoundedInteger)

--------------------------------------------------------------------------------

unminifiedCss :: Pattern
unminifiedCss = "static/css/*" .&&. complement "static/css/*.min.css"

simpleStaticAssets :: Pattern
simpleStaticAssets = ("static/**" .&&. complement unminifiedCss) .||. "robots.txt"

main :: IO ()
main = do
    -- because anything other than UTF-8 is ridiculous.
    E.setLocaleEncoding E.utf8
    hakyll hakyllRules

hakyllRules :: Rules ()
hakyllRules = do
    match simpleStaticAssets $ do
        route   idRoute
        compile copyFileCompiler

    match "index.html" $ do
        route idRoute
        -- note: demoteHeaders decodes entities, which we don't want
        let ctx = snippetField <> defaultContext 
        compile $ getResourceBody >>= applyAsTemplate ctx

    match unminifiedCss $ do
        route idRoute
        compile compressCssCompiler

    match "contact.html" $ do
        route idRoute
        let ctx = constField "title" "Contact me" 
                <> snippetField
                <> copyrightContext
                <> defaultContext 

        compile $ getResourceBody >>= applyAsTemplate ctx 
                  >>= loadAndApplyTemplate "templates/default.html" ctx

    create ["about.html"] $ do
        route idRoute
        compile $ do
            sections <- sortByMetadata "order" =<< loadAll "about/*.html"

            let aboutSecCtx = iconContext <> labelContext <> defaultContext
            let aboutCtx 
                    = listField "about-sections" aboutSecCtx (return sections)
                    <> constField "title" "About Me"
                    <> constField "extrastyle" "about.css"
                    <> copyrightContext
                    <> defaultContext

            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/about.html" aboutCtx
                >>= loadAndApplyTemplate "templates/default.html" aboutCtx

    -- Loosely based on https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("blog/**/*.md" .&&. hasNoVersion)
            specialPages <- loadAll (fromList ["about.html", "contact.html"])
            let postListIds = "blog/pagelist/*.html"
            blogPostLists <- liftM2 (:) (load "blog.html") (loadAll postListIds)
            let pages = specialPages ++ blogPostLists ++ posts
            let rootCtx = constField "rootUrl" rootUrl
            let pgCtx = listField "pages" (rootCtx <> defaultContext) (return pages)
            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/sitemap.xml" (rootCtx <> pgCtx)

    -- Build tags and tag summary pages
    tags <- buildTags ("blog/**/*.md" .&&. hasNoVersion) (fromCapture tagPagePattern)
    -- this generates the actual tag summary pages
    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let tagSummary = tagSummaryId tag
            let ctx = constField "tag" tag
                    -- read summary
                    <> field "tag-summary" (const $ loadBody tagSummary)
                    <> listField "posts" (postCtx tags) (return posts)
                    <> seriesCtx tag
                    <> constField "title" ("Entries tagged \"" ++ tag ++ "\"")
                    <> copyrightContext
                    <> keywordsContext
                    <> defaultContext

            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/tag-overview.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    blogPagination <- do
        let grp = liftM (paginateEvery 4) . sortRecentFirst
        buildPaginateWith grp ("blog/**/*.md" .&&. hasNoVersion) blogPageId

    paginateRules blogPagination $ \page pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let postInListCtx = postCtxNoTags <> field "preview" getPreview
            let ctx = constField "title" "Blog"
                    <> listField "posts" postInListCtx (return posts)
                    <> radialPaginationContext 2 blogPagination page
                    <> copyrightContext
                    <> defaultContext
            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    match "blog/**/*.md" $ do
        route $ setExtension "html"
        let ctx = field "article-meta" jsonldMetaForItem <> postCtx tags
        compile $ pandocBlogPostCompiler
            >>= loadAndApplyTemplate "templates/post.html" ctx 
            >>= loadAndApplyTemplate "templates/default.html" ctx

    match "blog/**/*.md" $ version "preview" $ compile pandocPreviewCompiler

    match "blog/**/*.md" $ version "jsonld-meta" $ do
        -- override the url field to point to the base version
        let ctx = field "abs-url" (absoluteUri . forBaseVer) 
                <> field "url" (routeOrFail . forBaseVer) <> postCtx tags 
        compile $ makeItem ("" :: String)
            >>= loadAndApplyTemplate "templates/jsonld/article-info.json" ctx

    -- offer mathjax alternative for browsers that don't support MathML
    -- (looking at you, Chromium derivatives)
    matchMetadata "blog/**/*.md" usesMath $ version "mathjax" $ do
        route $ setExtension "mathjax.html"
        let ctx = field "article-meta" jsonldMetaForItem
                <> constField "mathjax" "true"
                <> field "url" (routeOrFail . forBaseVer)
                <> postCtx tags
        let wo = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }
        compile $ pandocBlogPostCompiler' wo
            >>= loadAndApplyTemplate "templates/post.html" ctx 
            >>= loadAndApplyTemplate "templates/default.html" ctx

    match "templates/**" $ compile templateBodyCompiler
    match "snippets/**" $ compile getResourceBody
    -- these templates contain just the summary text; they're optional
    -- and not routed
    match "tag-summaries/*.md" $ compile pandocCompiler
    matchMetadata "tag-summaries/*" isSeries $ version "jsonld-meta" $ do
            let ctx = field "url" (routeOrFail . tagPageFromSummary)
                    <> field "abs-url" (absoluteUri . tagPageFromSummary) 
                    <> listFieldWith "parts"  seriesPartContext (seriesParts tags)
                    <> defaultContext
            compile $ makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/jsonld/series-info.json" ctx

    matchMetadata "tag-summaries/*" isSeries $ version "series-ref" $ do
        let ctx = field "url" (routeOrFail . tagPageFromSummary)
                <> field "abs-url" (absoluteUri . tagPageFromSummary) 
                <> defaultContext
        compile $ makeItem ("" :: String)
            >>= loadAndApplyTemplate "templates/jsonld/series-ref.json" ctx

    matchMetadata "tag-summaries/*" isSeries $ version "series-info" $ do
        let ctx = field "url" (routeOrFail . tagPageFromSummary) <> defaultContext
        compile $ makeItem ("" :: String)
            >>= loadAndApplyTemplate "templates/series-info.html" ctx

    match projInfoPattn $ compile projectInfoCompiler
    match "about/*.html" $ compile $ do
        let ctx = snippetField <> functionField "projectinfo" projInfo
        getResourceBody >>= applyAsTemplate ctx
    match "about/*.css" $ compile compressCssCompiler
    match "biblio/*.csl" $ compile cslCompiler
    match "biblio/*.csl" $ version "biblio-publish" $ do
        route (gsubRoute "biblio/" (const "static/biblio/"))
        compile copyFileCompiler
    where isSeries meta = lookupString "series" meta == Just "true"
          usesMath meta = lookupString "math" meta == Just "true"
          forBaseVer = setVersion Nothing . itemIdentifier
          tagFromSummary = takeBaseName . toFilePath . itemIdentifier
          tagPageFromSummary = fromCapture tagPagePattern . tagFromSummary
          seriesParts tags item = do
                -- manually extract tag pattern from tagMap
                let maybeIdents = snd <$> find predicate (tagsMap tags)
                case maybeIdents of
                    Nothing -> fail "Failed to extract tag pattern"
                    Just idents -> chronological =<< loadAll (fromList idents)
            where predicate (tag, _) = tag == tagFromSummary item
          seriesPartContext = field "abs-url" (absoluteUri . itemIdentifier) 
                            <> defaultContext
          projInfo :: [String] -> Item a -> Compiler String
          projInfo [projId] _ = loadBody (fromCapture projInfoPattn projId)
          projInfo _ _ = fail "Wrong number of arguments to projectinfo()"

          projInfoPattn :: Pattern
          projInfoPattn = "about/projects/*.md" 

          blogPageId :: PageNumber -> Identifier
          blogPageId 1 = "blog.html"
          blogPageId n = fromFilePath $ "blog/pagelist/" ++ show n ++ ".html"
          getPreview = loadBody . setVersion (Just "preview") . itemIdentifier


--------------------------------------------------------------------------------

rootUrl :: String
rootUrl = "https://mvalvekens.be"

tagPagePattern :: Pattern
tagPagePattern = "tags/*.html"

tagSummaryId :: String -> Identifier
tagSummaryId tag = fromFilePath $ "tag-summaries/" ++ tag ++ ".md"

getStringFromMeta :: String -> Identifier -> Compiler String
getStringFromMeta entryKey ident = do
    metadata <- getMetadata ident
    case lookupString entryKey metadata of
        Nothing -> noResult $ "No " ++ entryKey ++ " in metadata"
        Just value -> return value

fieldFromItemMeta :: String -> Context String
fieldFromItemMeta name = field name $ getStringFromMeta name . itemIdentifier


jsonldMetaFor :: Identifier -> Compiler String
jsonldMetaFor = loadBody . setVersion (Just "jsonld-meta")

jsonldMetaForItem :: Item a -> Compiler String
jsonldMetaForItem = jsonldMetaFor . itemIdentifier


routeOrFail :: Identifier -> Compiler String
routeOrFail ident = do
    maybeRoute <- getRoute ident
    case maybeRoute of
        Nothing -> fail $ "No route to '" ++ show ident ++ "'."
        Just x -> return ("/" ++ x)

absoluteUri :: Identifier -> Compiler String
absoluteUri = fmap (rootUrl++) . routeOrFail


seriesCtx :: String -> Context String
seriesCtx tag = field "series-meta" (const meta)
              <> field "title" (const getTitle)
    where getTitle = getStringFromMeta "title" (tagSummaryId tag)
          meta = jsonldMetaFor (tagSummaryId tag)

seriesPartCtx :: Context String
seriesPartCtx = field "series-meta" getSeriesMeta
              <> field "series" getSeriesInfo
              <> fieldFromItemMeta "series-part"
              <> fieldFromItemMeta "part-title"
    where getSeriesMeta item = do
             seriesTag <- getStringFromMeta "series" (itemIdentifier item)
             let tsi = tagSummaryId seriesTag
             loadBody (setVersion (Just "series-ref") tsi)
          getSeriesInfo item = do
             seriesTag <- getStringFromMeta "series" (itemIdentifier item)
             let tsi = tagSummaryId seriesTag
             loadBody (setVersion (Just "series-info") tsi)


mathCtx :: Context String
mathCtx = fieldFromItemMeta "math"
        <> field "mathjax-ver-url" (routeOrFail . mathjax . itemIdentifier)
  where mathjax = setVersion (Just "mathjax")


postCtxNoTags :: Context String
-- Use the One True Date Order (YYYY-mm-dd)
postCtxNoTags = dateField "date" "%F" 
             <> mathCtx
             <> seriesPartCtx
             <> keywordsContext
             <> copyrightContext
             <> defaultContext

postCtx :: Tags -> Context String
postCtx tags =  tagsField "tags" tags <> postCtxNoTags

iconContext :: Context String
iconContext = fieldFromItemMeta "icon"

labelContext :: Context String
labelContext = fieldFromItemMeta "label"

-- grab keywords from a tag summary
-- if there are no keywords --> use the tag itself
keywordsForTag :: String -> Compiler [String]
keywordsForTag tag = do
    metadata <- getMetadata (tagSummaryId tag)
    return $ fromMaybe [tag] (lookupStringList "keywords" metadata)

keywordsContext :: Context String
keywordsContext = field "keywords" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    -- intention: tags is for blog posts, keywords for metadata
    let keywordsMeta = fromMaybe [] (lookupStringList "keywords" metadata)
    let tagsMeta = fromMaybe [] (lookupStringList "tags" metadata)
    -- grab keywords for all tag summaries
    tagsKw <- traverse keywordsForTag tagsMeta
    -- only retain uniques
    -- (nub is fine, since we don't expect these lists to be very long)
    let keywords = nub $ concat $ (keywordsMeta:tagsKw)
    if null keywords then noResult "No keywords" 
                     else (return $ intercalate ", " keywords)

mainAuthor :: String
mainAuthor = "Matthias Valvekens"

getItemAuthor :: Item a -> Compiler (Maybe String)
getItemAuthor item = getMetadata (itemIdentifier item)
        >>= return . lookupString "author"

copyrightContext :: Context String
copyrightContext = licenseContext
        <> field "author" (getItemAuthor >=> checkAuthor)
        <> fieldFromItemMeta "author-url"
        <> field "copyrightline" (fmap cline . getItemAuthor)
    where cline Nothing = mainAuthor
          cline (Just author)
            | author == mainAuthor = mainAuthor
            | otherwise = author <> ", " <> mainAuthor
          checkAuthor Nothing = noResult "No author"
          checkAuthor (Just author) = return author

data LicenseInfo = LicenseInfo 
    { licenseSnippetId :: Identifier
    , licenseUrl :: String }

licInfo' :: String -> Maybe LicenseInfo
licInfo' "CC BY-SA 4.0" = Just $ LicenseInfo
    { licenseSnippetId = "snippets/copyright/cc-by-sa-4.0.html"
    , licenseUrl = "https://creativecommons.org/licenses/by-sa/4.0/" }
licInfo' "CC BY-SA 4.0 hybrid" = Just $ LicenseInfo
    { licenseSnippetId = "snippets/copyright/cc-by-sa-4.0-code-apache.html"
    , licenseUrl = "https://creativecommons.org/licenses/by-sa/4.0/" }
licInfo' _ = Nothing

licenseInfo :: Identifier -> Compiler LicenseInfo
licenseInfo ident = do
    declaredLicense <- getStringFromMeta "license" ident
    case licInfo' declaredLicense of
        Nothing -> fail $ 
            "License " ++ declaredLicense 
                ++ " is not known to the templating engine"
        Just x -> return x

licenseContext :: Context String
licenseContext 
        = (field "license" $ licForItem >=> loadBody . licenseSnippetId)
        <> (field "license-url" $ licForItem >=> return . licenseUrl)
    where licForItem :: Item a -> Compiler LicenseInfo
          licForItem = licenseInfo . itemIdentifier

sortByMetadata :: (MonadMetadata m, MonadFail m) => String -> [Item a] -> m [Item a]
sortByMetadata theMeta = sortByM $ extract . itemIdentifier
  where
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs
    extract idt = do
        metadata <- getMetadata idt
        return $ lookupString theMeta metadata

-------------------------------------------------
-- Pandoc stuff for blog posts

-- Modified version of readPandocBiblio from Hakyll.Web.Pandoc.Biblio to use the
-- embedded "references" entry in the metadata of our post
readPandocBiblioFromMeta :: ReaderOptions -> Item CSL 
                         -> Item String -> Compiler (Item Pandoc)
readPandocBiblioFromMeta ropt csl item = do
    Pandoc.Pandoc (Pandoc.Meta meta) blocks <- itemBody <$>
        readPandocWith ropt item

    let cslFile = Pandoc.FileInfo zeroTime . unCSL $ itemBody csl
        addBiblioFiles = \st -> st
            { Pandoc.stFiles =
                Pandoc.insertInFileTree "_hakyll/style.csl" cslFile $
                Pandoc.stFiles st
            }
        biblioMeta = Pandoc.Meta .
            Map.insert "csl" (Pandoc.MetaString "_hakyll/style.csl") $
            Map.insert "link-citations" (Pandoc.MetaBool True) $
            Map.insert "link-bibliography" (Pandoc.MetaBool False) $
            meta
        errOrPandoc = Pandoc.runPure $ do
            Pandoc.modifyPureState addBiblioFiles
            Pandoc.processCitations $ Pandoc.Pandoc biblioMeta blocks

    pandoc <- case errOrPandoc of
        Left  e -> compilerThrow ["Error during processCitations: " ++ show e]
        Right x -> return x

    return $ fmap (const pandoc) item
  where zeroTime = Time.UTCTime (toEnum 0) 0


shiftAndStyleHeadings :: Int -> Block -> Block
shiftAndStyleHeadings by (Header lvl attr content) = Header lvl' attr' content'
    where (elId, classes, kvals) = attr
          lvl' = lvl + by
          -- we up the level one more in Bulma styling
          classes' = ("subtitle":"is-" <> (T.pack $ show $ lvl' + 1):classes)
          attr' = (elId, classes', kvals)
          linkAttr = ("", ["section-link"], [])
          content' = [Link linkAttr content ("#" <> elId, "")]
shiftAndStyleHeadings _ x = x


grabJsonObj :: T.Text -> Compiler Aes.Object
grabJsonObj json = do
    case Aes.decodeStrict (encodeUtf8 json) of
        Just (Aes.Object jsonObj) -> return jsonObj
        Just _ -> fail "JSON type error: expected object"
        Nothing -> fail "JSON decoding failure"

jsonString :: Aes.ToJSON a => a -> String
jsonString = T.unpack . decodeUtf8 . toStrict . Aes.encode


formatInlineMetadata :: Block -> Compiler Block
formatInlineMetadata orig@(CodeBlock attr jsonMeta)
    | not ("meta" `elem` classes) = return orig
    | otherwise = do
        json <- grabJsonObj jsonMeta >>= insertDefault "@id" defaultId
        let ctx = constField "jsonld-meta" (jsonString json)
        metaItem <- makeItem ("" :: String) 
                        >>= loadAndApplyTemplate templateName ctx
        return $ RawBlock "html" $ T.pack $ itemBody metaItem
    where (elId, classes, _) = attr
          insertDefault k defaultVal = HMS.alterF alter k
            where alter Nothing = Just <$> defaultVal
                  alter (Just x) = return (Just x)
          templateName = "templates/jsonld/jsonld-meta.html"
          defaultId = do
            if elId == "" then fail "Inline metadata must have an ID" else pure ()
            currentUrl <- getUnderlying >>= routeOrFail
            return $ Aes.String (T.pack (rootUrl ++ currentUrl) <> "#" <> elId)
            

formatInlineMetadata orig = return orig


embedYoutubeVideos :: Block -> Compiler Block
embedYoutubeVideos orig@(CodeBlock attr jsonMeta)
    | not ("youtube" `elem` classes) = return orig
    | otherwise = do
        rawObj <- grabJsonObj jsonMeta
        width <- extractIntOrFail "width" rawObj
        height <- extractIntOrFail "height" rawObj
        title <- extractStringOrFail "name" rawObj
        ytid <- extractPandocAttr "ytid" kvals
        let videoUrl = "https://www.youtube.com/watch?v=" <> ytid
        let embedUrl = "https://www.youtube-nocookie.com/embed/" <> ytid
        let thumbnailUrl = "https://img.youtube.com/vi/" <> ytid <> "/maxresdefault.jpg"
        let contentUrl = "https://youtube.googleapis.com/v/" <> ytid
        let newObj = HMS.insert "embedUrl" (Aes.String embedUrl)
                   $ HMS.insert "thumbnailUrl" (Aes.String thumbnailUrl)
                   $ HMS.insert "contentUrl" (Aes.String contentUrl) rawObj
        let ctx = constField "width" (show width) <> constField "height" (show height)
                <> constField "embed-url" (T.unpack embedUrl)
                <> constField "video-url" (T.unpack videoUrl)
                <> constField "youtube-meta" (jsonString newObj)
                <> constField "title" title
        ytEmbedItem <- makeItem ("" :: String) >>= loadAndApplyTemplate templateName ctx
        let ytContent = RawBlock "html" $ T.pack $ itemBody ytEmbedItem
        return $ Div (elId, classes, []) [ytContent]
        
    where (elId, classes, kvals) = attr
          templateName = "templates/youtube-embed.html" 
          extractIntOrFail :: T.Text -> Aes.Object -> Compiler Int
          extractIntOrFail key obj = do
            case HMS.lookup key obj of
                Just (Aes.Number x) -> case toBoundedInteger x of
                    Nothing -> fail "Expected int in JSON, got something else"
                    Just y -> return y
                _ -> fail $ "No numeric key " ++ T.unpack key ++ " in YouTube meta"
          extractStringOrFail key obj = do
            case HMS.lookup key obj of
                Just (Aes.String x) -> return (T.unpack x)
                _ -> fail $ "No string key " ++ T.unpack key ++ " in YouTube meta"

embedYoutubeVideos orig = return orig


extractPandocAttr :: T.Text -> [(T.Text, T.Text)] -> Compiler T.Text
extractPandocAttr key kvals = case msum (trans <$> kvals) of
        Nothing -> fail $ "No attribute value for '" ++ T.unpack key ++ "'."
        Just x -> return x
    where trans (k, v) = if k == key then (Just v) else Nothing


pandocBlogPostCompiler' :: WriterOptions -> Compiler (Item String)
pandocBlogPostCompiler' wo = do
        csl <- load $ fromFilePath "biblio/bibstyle.csl"
        getResourceBody >>= readPandocBiblioFromMeta ro csl >>= processPandoc
    where ro = defaultHakyllReaderOptions
            { -- The following option enables citation rendering
              readerExtensions = enableExtension Ext_citations $ readerExtensions defaultHakyllReaderOptions
            }
          transform = walkPandocM $ return . shiftAndStyleHeadings 1 
                        >=> embedYoutubeVideos >=> formatInlineMetadata
          processPandoc = withItemBody transform >=> return . writePandocWith wo


pandocBlogPostCompiler :: Compiler (Item String)
pandocBlogPostCompiler = pandocBlogPostCompiler' wo
  where wo = defaultHakyllWriterOptions { writerHTMLMathMethod = MathML }


projectInfoCompiler :: Compiler (Item String)
projectInfoCompiler = pandocCompilerWithTransformM ro wo transform
    where wo = defaultHakyllWriterOptions
          ro = defaultHakyllReaderOptions
          transform = walkPandocM $ formatInlineMetadata >=> formatTitle
          formatTitle (Header 1 (_, classes, _) content) = do
                ident <- getUnderlying
                projectUrl <- T.pack <$> getStringFromMeta "project-url" ident
                title <- T.pack <$> getStringFromMeta "title" ident
                return $ Header 2 ("", classes, []) [Link ("", [], []) content (projectUrl, title)]
          formatTitle x = return x


data PageNumInfo = PageNumInfo 
                 { pniGetPageNum :: PageNumber
                 , pniGetPageUrl :: String }

radialPaginationContext :: PageNumber -> Paginate -> PageNumber -> Context a
radialPaginationContext rad p curPage = paginateContext p curPage 
                                      <> before <> after
    where -- for each number, build the URL to the page using getRoute
          pgNumItem :: PageNumber -> Compiler (Item PageNumInfo)
          pgNumItem n = do
            maybeRoute <- getRoute (paginateMakeId p n)
            case maybeRoute of
                Nothing -> fail $ "Couldn't retrieve URL for page " ++ show n
                Just rt -> makeItem $ PageNumInfo n (toUrl rt)

          -- turn a PageNumInfo into template fields
          pgNumCtx :: Context PageNumInfo
          pgNumCtx = field "pageNum" fmtPgNum <> field "pageUrl" fmtPgUrl
            where fmtPgNum = return . show . pniGetPageNum . itemBody
                  fmtPgUrl = return . pniGetPageUrl . itemBody
          lastPageNum = Map.size $ paginateMap p
          seqOrNoResult [] = noResult "No pages"
          seqOrNoResult xs = sequence xs
          before = pgsField <> elide
            where pgsField = listField "pagesBefore" pgNumCtx (seqOrNoResult pgs)
                  fstInSet = max 2 (curPage - rad)
                  elide = field "elideBefore" $ const $ do
                    case fstInSet > 2 && not (null pgs) of
                        True -> return "elide"
                        False -> noResult "no ellipsis"
                  pgs = [pgNumItem n | n <- [fstInSet .. curPage - 1]]
          after = pgsField <> elide
            where pgsField = listField "pagesAfter" pgNumCtx (seqOrNoResult pgs)
                  lastInSet = min (lastPageNum - 1) (curPage + rad)
                  elide = field "elideAfter" $ const $ do
                    case lastInSet < lastPageNum - 1 && not (null pgs) of
                        True -> return "elide"
                        False -> noResult "no ellipsis"
                  pgs = [pgNumItem n | n <- [curPage + 1 .. lastInSet]]


pandocPreviewCompiler :: Compiler (Item String)
pandocPreviewCompiler = getResourceBody >>= readPandoc >>= render
    where render = withItemBody reduceToFirstPara >=> return . writePandoc
          reduceToFirstPara (Pandoc _ blks) = reduceToFirstPara' blks
          reduceToFirstPara' [] = noResult "no preview"
          reduceToFirstPara' (x:xs) = case x of
            Para inl -> return (Pandoc (Meta mempty) [Plain $ filterInl inl])
            _ -> reduceToFirstPara' xs
          filterInl = fmap (walk transfInl)
          transfInl :: Inline -> Inline
          -- drop footnotes
          transfInl (Note _) = Span ("", [], []) []
          -- unlink links
          transfInl (Link _ inls _) = Span ("", [], []) inls
          transfInl x = x

