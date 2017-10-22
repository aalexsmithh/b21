{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ( (<>) )
import Hakyll
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Jasmine ( minify )

defCtx
  = defaultContext

staticPages
  = ["pages/about.md"]

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile compressJsCompiler

  match "font/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "files/*" $ do
    route   idRoute
    compile copyFileCompiler

  create staticPages $ do
    route $
      setExtension "html" `composeRoutes` customRoute (drop 6 . toFilePath)
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defCtx
      >>= relativizeUrls

  -- create ["atom.xml"] $ do
  --   route idRoute
  --   compile $ do
  --     let feedCtx = postCtx <> bodyField "description"
  --     posts <- fmap (take 10) . recentFirst
  --       =<< loadAllSnapshots "posts/*" "content"
  --     renderAtom feedConfig feedCtx posts

  match "testimonials/*" $ do
    -- no route because we don't ever have these on their own pages
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/testimonial.html" defCtx
      >>= relativizeUrls

  match "projects/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/project.html" defCtx
      -- >>= saveSnapshot "content"
      -- >>= loadAndApplyTemplate "templates/default.html" defCtx
      -- >>= relativizeUrls

  match "pages/projects.html" $ do
    route $ constRoute "projects.html"

    compile $ do
      projects <- loadAll "projects/*" -- "content"
      ctx <- pure $
        listField "projects" defCtx (pure projects)
        <> defCtx

      getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

  match "pages/index.html" $ do
    route $ constRoute "index.html"
    compile $ do
      testimonials <- loadAll "testimonials/*"

      ctx <- pure $
        listField "testimonials" defCtx (pure testimonials)
        <> defCtx

      getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minjs = T.unpack . T.decodeUtf8 . LBS.toStrict . minify . LBS.fromStrict . T.encodeUtf8 . T.pack . itemBody
  s <- getResourceString
  pure $ itemSetBody (minjs s) s
