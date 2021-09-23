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
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Monad (liftM)
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
    -- these templates contain just the summary text; they're optional and not routed
    match "tag-summaries/*.md" $ compile pandocCompiler
    -- this generates the actual tag summary pages
    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let tagSummary = fromFilePath $ "tag-summaries/" ++ tag ++ ".md"
            let ctx = constField "title" ("Entries tagged \"" ++ tag ++ "\"")
                    <> constField "tag" tag
                    -- read summary
                    <> field "tag-summary" (const $ loadBody tagSummary)
                    <> listField "posts" (postCtx tags) (return posts)
                    <> copyrightContext
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
        let ctx = postCtx tags
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


postCtx :: Tags -> Context String
postCtx tags =  tagsField "tags" tags 
             -- Use the One True Date Order (YYYY-mm-dd)
             <> dateField "date" "%F" 
             <> copyrightContext
             <> defaultContext

iconContext :: Context String
iconContext = field "icon" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ lookupString "icon" metadata

labelContext :: Context String
labelContext = field "label" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ lookupString "label" metadata


mainAuthor :: String
mainAuthor = "Matthias Valvekens"

getItemAuthor :: Item a -> Compiler (Maybe String)
getItemAuthor item = getMetadata (itemIdentifier item)
                    >>= return . lookupString "author"

copyrightContext :: Context String
copyrightContext = licenseContext
        <> field "author" (fmap (fromMaybe "") . getItemAuthor)
        <> field "copyrightline" (fmap cline . getItemAuthor)
    where cline Nothing = mainAuthor
          cline (Just author)
            | author == mainAuthor = mainAuthor
            | otherwise = author <> ", " <> mainAuthor

licenseContext :: Context String
licenseContext = field "license" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    case lookupString "license" metadata of
        Just "CC BY-SA 4.0" -> loadBody "snippets/copyright/cc-by-sa-4.0.html"
        -- TODO add a template for CC BY-SA content with differently licensed code snippets
        -- (I don't want to accidentally require CC compliance for code)
        _ -> return ""
        

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
