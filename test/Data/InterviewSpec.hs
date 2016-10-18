{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.InterviewSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Interview
import Data.Text (Text, pack)
import Data.String.Here (hereFile)
import Text.Subtitles.SRT
import Data.Attoparsec.Text (parseOnly, endOfInput)

exSRTtext :: Text
exSRTtext = pack [hereFile|captions.srt|]

line1 = Line { index = 1
             , range = Range {from = Time {hour = 0, minutes = 0, seconds = 0, frame = 0}
                             , to = Time {hour = 0, minutes = 0, seconds = 8, frame = 550}}
             , geometry = Nothing
             , dialog = "hopefully this works"}

line2 = Line { index = 2
             , range = Range {from = Time {hour = 0, minutes = 0, seconds = 3, frame = 449}
                             , to = Time {hour = 0, minutes = 0, seconds = 11, frame = 790}}
             , geometry = Nothing
             , dialog = "yeah i know i mean i I uh I haven't uh"}

line1' = Line { index = 1
              , range = Range {from = Time {hour = 0, minutes = 0, seconds = 4, frame = 449}
                              , to = Time {hour = 0, minutes = 0, seconds = 11, frame = 790}}
              , geometry = Nothing
              , dialog = "we could have done this in person"}



parsedSRT :: Subtitles
parsedSRT = [ line1
            , line2
            ]

parsedSRT2 :: Subtitles
parsedSRT2 = [ line1' ]

speaker1 :: Participant
speaker1 = Participant "Me"

speaker2 :: Participant
speaker2 = Participant "Other"

singleLine :: Line
singleLine = Line { index = 1
                  , range = Range { from = Time {hour = 0, minutes = 0, seconds = 0, frame = 0}
                                  , to = Time {hour = 0, minutes = 0, seconds = 11, frame = 790}}
                   , geometry = Nothing
                   , dialog = "hopefully this works yeah i know i mean i I uh I haven't uh"}


exampleInterview :: Interview
exampleInterview =
  [ (speaker1, singleLine)
  , (speaker2, line1' {index = 2})
  ]

exampleInterviewHTML =
  "<p><span class=\"timestamp\" data-timestamp=\"0:00\" contenteditable=\"false\">0:00</span><br />\n<em><span class=\"speaker\" data-speaker=\"Me\">Me</span></em>: hopefully this works yeah i know i mean i I uh I haven't uh</p>\n<p><br />\n</p>\n<p><span class=\"timestamp\" data-timestamp=\"0:04\" contenteditable=\"false\">0:04</span><br />\n<em><span class=\"speaker\" data-speaker=\"Other\">Other</span></em>: we could have done this in person</p>"

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseSRT" $ do
    it "can parse a file" $ do
      parseOnly (parseSRT <* endOfInput) exSRTtext `shouldBe` Right parsedSRT
  describe "mergeLines" $ do
    it "can merge multiple lines by the same speaker" $
      mergeLines parsedSRT `shouldBe` singleLine
  describe "makeInterview" $ do
    it "weaves together an interview from different speakers" $ do
      let inputData = [(speaker1, parsedSRT), (speaker2, parsedSRT2)]
      makeInterview inputData `shouldBe` exampleInterview
  describe "parseArgs" $ do
    it "parses command-line arguments into a list of participants and files" $
      parseArgs ["Me", "me.srt", "Other", "other.srt"] `shouldBe`
        [(Participant "Me", "me.srt"), (Participant "Other", "other.srt")]
  describe "toOTR" $ do
    it "converts an interview to an OTR record" $ do
      toOTR exampleInterview `shouldBe` OTR { otrText = exampleInterviewHTML
                                            , otrMedia = ""
                                            , otrMedia_time = 0.0
                                            , otrMedia_source = ""
                                            }

