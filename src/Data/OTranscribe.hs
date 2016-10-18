{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.OTranscribe where

import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import Data.Aeson.TH
import Text.Pandoc
import Text.Subtitles.SRT.Datatypes
import Data.Char (toLower)
import Text.Pandoc.Builder

data OTR = OTR
  { otrText :: String
  , otrMedia :: String
  , otrMedia_time :: Float
  , otrMedia_source :: String
  } deriving (Eq, Show, Ord)

$(deriveJSON defaultOptions{
     fieldLabelModifier = map (\x -> if x == '_' then '-' else x) . drop 3 . map toLower
     } ''OTR)

toOtrTimestamp :: Time -> Inlines
toOtrTimestamp t =
  spanWith ("", ["timestamp"], []) (text t')
  where
    h = hour t
    h' = if h > 0 then show h <> ":" else ""
    m = minutes t
    s = seconds t
    t' = h' <> show m <> ":" <> show s
