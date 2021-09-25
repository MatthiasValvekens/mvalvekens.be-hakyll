--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Hakyll

import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkPandocM)
import Text.Pandoc 
    ( Extension (..)
    , ReaderOptions (..)
    , enableExtension )
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Citeproc as Pandoc (processCitations)
import Hakyll.Core.Compiler.Internal (compilerThrow)

import Data.Monoid (mappend)
import Data.Ord (comparing)
import Data.List (sortBy, nub, intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Monad (liftM, (>=>))
import Control.Monad.Identity (runIdentity)

import qualified GHC.IO.Encoding as E


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
        compile $ do
            getResourceBody
                >>= applyAsTemplate (snippetField <> defaultContext)
                >>= relativizeUrls

    match unminifiedCss $ do
        route idRoute
        compile compressCssCompiler

    create ["contact.html"] $ do
        route idRoute
        compile $ do
            let ctx = constField "title" "Contact me" 
                    <> copyrightContext
                    <> defaultContext 

            body <- loadBody "snippets/contact-info.html"
            makeItem body
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls 

    create ["about.html"] $ do
        route idRoute
        compile $ do
            sections <- sortByMetadata "order" =<< loadAll "about/*"

            let aboutSecCtx = iconContext <> labelContext <> defaultContext
            let aboutCtx 
                    = listField "about-sections" aboutSecCtx (return sections)
                    <> constField "title" "About Me"
                    <> copyrightContext
                    <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/about.html" aboutCtx
                >>= loadAndApplyTemplate "templates/default.html" aboutCtx
                >>= relativizeUrls

    -- Loosely based on https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/**"
            specialPages <- loadAll (fromList ["about.html", "contact.html"])
            let pages = specialPages ++ posts
            let rootCtx = constField "rootUrl" rootUrl
            let pgCtx = listField "pages" (rootCtx <> defaultContext) (return pages)
            makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" (rootCtx <> pgCtx)

    -- Build tags and tag summary pages
    tags <- buildTags "blog/**" (fromCapture "tags/*.html")
    -- these templates contain just the summary text; they're optional
    -- and not routed
    match "tag-summaries/*.md" $ compile pandocCompiler
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

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag-overview.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "blog.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/**"
            let ctx = listField "posts" (postCtx tags) (return posts)
                    <> copyrightContext
                    <> defaultContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "blog/**" $ do
        route $ setExtension "html"
        let ctx = keywordsContext <> postCtx tags
        compile $ pandocBlogPostCompiler
            >>= loadAndApplyTemplate "templates/post.html" ctx 
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
    match "snippets/**" $ compile getResourceBody
    match "tag-summaries/*" $ compile getResourceBody
    match "about/*" $ compile $ do
        getResourceBody >>= applyAsTemplate snippetField
    match "biblio/*.csl" $ compile cslCompiler
    match "biblio/*.csl" $ version "biblio-publish" $ do
        route (gsubRoute "biblio/" (const "static/biblio/"))
        compile copyFileCompiler


--------------------------------------------------------------------------------

rootUrl :: String
rootUrl = "https://mvalvekens.be"

tagSummaryId :: String -> Identifier
tagSummaryId tag = fromFilePath $ "tag-summaries/" ++ tag ++ ".md"

tagSummaryUrl :: String -> String
tagSummaryUrl tag = "/tags/" ++ tag ++ ".html"


getStringFromMeta :: String -> Identifier -> Compiler String
getStringFromMeta entryKey ident = do
    metadata <- getMetadata ident
    case lookupString entryKey metadata of
        Nothing -> noResult $ "No " ++ entryKey ++ " in metadata"
        Just value -> return value

fieldFromItemMeta :: String -> Context String
fieldFromItemMeta name = field name $ getStringFromMeta name . itemIdentifier


fmtSeriesInfo :: Identifier -> String -> Compiler String
fmtSeriesInfo template seriesTag = do
     let tagSummary = tagSummaryId seriesTag
     seriesMeta <- getMetadata tagSummary
     let seriesUrl = rootUrl <> tagSummaryUrl seriesTag
     let ctx = constField "series-url" seriesUrl <> defaultContext
     result <- load tagSummary >>= loadAndApplyTemplate template ctx
     return (itemBody result)


seriesCtx :: String -> Context String
seriesCtx tag = field "series-meta" (const meta)
              <> field "title" (const getTitle)
    where meta = fmtSeriesInfo "templates/series-info.json" tag
          getTitle = getStringFromMeta "title" (tagSummaryId tag)

seriesPartCtx :: Context String
seriesPartCtx = field "series-meta" getSeriesMeta
              <> field "series" getSeriesInfo
              <> fieldFromItemMeta "series-part"
              <> fieldFromItemMeta "part-title"
    where getSeriesMeta item = do
             seriesTag <- getStringFromMeta "series" (itemIdentifier item)
             fmtSeriesInfo "templates/series-info.json" seriesTag
          getSeriesInfo item = do
             seriesTag <- getStringFromMeta "series" (itemIdentifier item)
             fmtSeriesInfo "templates/series-info.html" seriesTag

postCtx :: Tags -> Context String
postCtx tags =  tagsField "tags" tags 
             -- Use the One True Date Order (YYYY-mm-dd)
             <> dateField "date" "%F" 
             <> seriesPartCtx
             <> copyrightContext
             <> defaultContext

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

-- TODO add a template for CC BY-SA content with differently 
-- licensed code snippets
-- (I don't want to accidentally require CC compliance for code)
licInfo' :: String -> Maybe LicenseInfo
licInfo' "CC BY-SA 4.0" = Just $ LicenseInfo
    { licenseSnippetId = "snippets/copyright/cc-by-sa-4.0.html"
    , licenseUrl = "https://creativecommons.org/licenses/by-sa/4.0/" }
licInfo' x = Nothing

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
shiftAndStyleHeadings by (Header lvl attr content) = Header lvl' attr' content
    where (elId, classes, kvals) = attr
          lvl' = lvl + by
          -- we up the level one more in Bulma styling
          classes' = ("subtitle":"is-" <> (T.pack $ show $ lvl' + 1):classes)
          attr' = (elId, classes', kvals)
shiftAndStyleHeadings _ x = x


pandocBlogPostCompiler :: Compiler (Item String)
pandocBlogPostCompiler = do
        csl <- load $ fromFilePath "biblio/bibstyle.csl"
        liftM processPandoc (getResourceBody >>= readPandocBiblioFromMeta ro csl)
    where wo = defaultHakyllWriterOptions
          ro = defaultHakyllReaderOptions
            { -- The following option enables citation rendering
              readerExtensions = enableExtension Ext_citations $ readerExtensions defaultHakyllReaderOptions
            }
          transform = runIdentity . (walkPandocM $ return . shiftAndStyleHeadings 1)
          processPandoc = writePandocWith wo . fmap transform
