--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
import Data.Monoid (mappend)
import Hakyll
import Control.Monad (liftM)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)


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

    -- TODO put publication list in markdown
    -- TODO rewrite index using more robust templating logic?
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = contactInfoCtx <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= relativizeUrls

    match unminifiedCss $ do
        route   idRoute
        compile compressCssCompiler

    create ["contact.html"] $ do
        route idRoute
        compile $ do
            let ctx = constField "title" "Contact" <> defaultContext 

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


    match "templates/*" $ compile templateBodyCompiler
    match "snippets/contact-info.html" $ compile getResourceBody
    match "about/*" $ compile getResourceBody


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


iconContext :: Context a
iconContext = field "icon" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ lookupString "icon" metadata

labelContext :: Context a
labelContext = field "label" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ lookupString "label" metadata

contactInfoCtx :: Context String
contactInfoCtx = field "contact" (const $ loadBody "snippets/contact-info.html")


sortByMetadata :: (MonadMetadata m, MonadFail m) => String -> [Item a] -> m [Item a]
sortByMetadata theMeta = sortByM $ extract . itemIdentifier
  where
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs
    extract idt = do
        metadata <- getMetadata idt
        return $ lookupString theMeta metadata
        
