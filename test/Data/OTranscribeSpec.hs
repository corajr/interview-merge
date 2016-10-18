{-# LANGUAGE OverloadedStrings #-}
module Data.OTranscribeSpec (main, spec) where

import Test.Hspec
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Subtitles.SRT.Datatypes
import Data.OTranscribe
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)

exTime :: Time
exTime = Time { hour = 0, minutes = 1, seconds = 11, frame = 0}

exTimestamp :: Inlines
exTimestamp = spanWith ("", ["timestamp"], []) (text "1:11")

otrEx :: OTR
otrEx = OTR
  { otrText = "<p>example</p>"
  , otrMedia = ""
  , otrMedia_time = 0.0
  , otrMedia_source = ""
  }

otrExJson :: ByteString
otrExJson = "{\"text\":\"<p>example</p>\",\"media\":\"\",\"media-time\":0,\"media-source\":\"\"}"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toOtrTimestamp" $
    it "turns a Text.Subtitles.SRT Time into an OTR timestamp" $
      toOtrTimestamp exTime `shouldBe` exTimestamp
  describe "encode OTR" $
    it "encodes an OTR example" $
      encode otrEx `shouldBe` otrExJson
