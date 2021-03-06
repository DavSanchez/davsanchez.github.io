{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hakyll
import Text.Pandoc.Options

main :: IO ()
main =
  hakyllWith config $ do
    match "assets/*" $ do
      route idRoute
      compile copyFileCompiler

    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
      route (setExtension "html")
      compile $
        pandocMathCompiler -- Changed from pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "posts/*" $ do
      route (setExtension "html")
      compile $
        pandocMathCompiler -- Changed from pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postContext
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" postContext
          >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveContext =
              listField "posts" postContext (return posts)
                `mappend` constField "title" "Archives"
                `mappend` defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveContext
          >>= loadAndApplyTemplate "templates/default.html" archiveContext
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexContext =
              listField "posts" postContext (return posts)
                `mappend` constField "title" "Blog"
                `mappend` defaultContext

        getResourceBody
          >>= applyAsTemplate indexContext
          >>= loadAndApplyTemplate "templates/default.html" indexContext
          >>= relativizeUrls

    match "templates/*" (compile templateCompiler)

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        posts <-
          fmap (take 10) . recentFirst
            =<< loadAllSnapshots "posts/*" "content"
        renderAtom feedConfiguration feedContext posts

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        posts <-
          fmap (take 10) . recentFirst
            =<< loadAllSnapshots "posts/*" "content"
        renderRss feedConfiguration feedContext posts

feedContext :: Context String
feedContext =
  postContext `mappend` bodyField "description"

postContext :: Context String
postContext =
  dateField "date" "%Y-%m-%d" `mappend` defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "DavSanchez",
      feedDescription = "DavSanchez's blog",
      feedAuthorName = "DavSanchez",
      feedAuthorEmail = "",
      feedRoot = ""
    }

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "public"
    }

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  let mathExtensions =
        [ Ext_tex_math_dollars,
          Ext_tex_math_double_backslash,
          Ext_latex_macros
        ]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr enableExtension defaultExtensions mathExtensions
      writerOptions =
        defaultHakyllWriterOptions
          { writerExtensions = newExtensions,
            writerHTMLMathMethod = MathJax ""
          }
   in pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- -- Math + Bib?
-- -- In the main function of site.hs
-- match "assets/*.bib" $ compile biblioCompiler
-- match "assets/*.csl" $ compile cslCompiler
-- -- Somewhere at the top level of site.hs
-- customPandocCompiler :: Compiler (Item String)
-- customPandocCompiler = do
--     csl <- load $ fromFilePath "assets/apa.csl"
--     bib <- load $ fromFilePath "assets/citations.bib"
--     fmap write (getResourceString >>= read csl bib)
--     where
--         read = readPandocBiblio defaultHakyllReaderOptions
--         write = writePandocWith writerOptions