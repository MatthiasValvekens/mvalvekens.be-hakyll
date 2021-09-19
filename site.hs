--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Hakyll

import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkPandocM)

import Data.Monoid (mappend)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Control.Monad (liftM)
import Control.Monad.Identity (runIdentity)


--------------------------------------------------------------------------------

unminifiedCss :: Pattern
unminifiedCss = "static/css/*" .&&. complement "static/css/*.min.css"

simpleStaticAssets :: Pattern
simpleStaticAssets = ("static/**" .&&. complement unminifiedCss) .||. "robots.txt"

main :: IO ()
main = hakyll $ do
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
            let ctx = constField "title" "Contact me" <> defaultContext 

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
                    <> defaultContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "blog/**" $ do
        route $ setExtension "html"
        compile $ pandocBlogPostCompiler
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
    match "snippets/*" $ compile getResourceBody
    match "tag-summaries/*" $ compile getResourceBody
    match "about/*" $ compile $ do
        getResourceBody >>= applyAsTemplate snippetField


--------------------------------------------------------------------------------

rootUrl :: String
rootUrl = "https://mvalvekens.be"


postCtx :: Tags -> Context String
postCtx tags =  tagsField "tags" tags 
             -- Use the One True Date Order (YYYY-mm-dd)
             <> dateField "date" "%F" <> defaultContext

iconContext :: Context String
iconContext = field "icon" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ lookupString "icon" metadata

labelContext :: Context String
labelContext = field "label" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ lookupString "label" metadata

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


shiftAndStyleHeadings :: Int -> Block -> Block
shiftAndStyleHeadings by (Header lvl attr content) = Header lvl' attr' content
    where (elId, classes, kvals) = attr
          lvl' = lvl + by
          -- we up the level one more in Bulma styling
          classes' = ("subtitle":"is-" <> (T.pack $ show $ lvl' + 1):classes)
          attr' = (elId, classes', kvals)
shiftAndStyleHeadings _ x = x

pandocBlogPostCompiler :: Compiler (Item String)
pandocBlogPostCompiler = pandocCompilerWithTransform ro wo transform
    where wo = defaultHakyllWriterOptions
          ro = defaultHakyllReaderOptions
          transform = runIdentity . (walkPandocM $ return . shiftAndStyleHeadings 1)
