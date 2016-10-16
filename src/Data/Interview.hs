{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Data.Interview where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Arrow (second, right)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Attoparsec.Text (parseOnly, endOfInput)
import System.Environment (getArgs)
import Text.Subtitles.SRT

newtype Participant = Participant
  { participantName :: Text }
  deriving (Eq, Show, Ord)

type Turn = (Participant, Line)

type Interview = [Turn]

mergeLines :: [Line] -> Line
mergeLines [] = error "mergeLines given empty list!"
mergeLines (x:[]) = x
mergeLines lines@(x:_) =
  Line { index = index x
       , range = Range { from = from (range x), to = finish}
       , geometry = geometry x
       , dialog = joinedDialog
       }
  where
    finish = to (range (last lines))
    joinedDialog = T.intercalate " " (map dialog lines)

makeInterview :: [(Participant, Subtitles)] -> Interview
makeInterview lines = renumberedTurns
  where turns = concatMap (\(p, xs) -> [(p, x) | x <- xs]) lines
        sortedTurns = sortBy (compare `on` (from . range . snd)) turns
        groupedTurns = map turnListToLines . groupBy ((==) `on` fst) $ sortedTurns
        mergedTurns = map (second mergeLines) groupedTurns
        renumberedTurns = zipWith (\(p,x) i -> (p, x { index = i })) mergedTurns [1..]
        turnListToLines :: [Turn] -> (Participant, [Line])
        turnListToLines ((p,x):xs) = (p, x:map snd xs)

parseArgs :: [String] -> [(Participant, FilePath)]
parseArgs [] = []
parseArgs (x:y:xs) = (Participant (T.pack x), y) : parseArgs xs

readInterview :: (Participant, FilePath) -> IO (Either String (Participant, Subtitles))
readInterview (p,x) = readFile x >>= (return . right (p,) . parseOnly (parseSRT <* endOfInput) . T.pack)

appMain :: IO ()
appMain = do
  args <- getArgs
  let participantFiles = parseArgs args
  input <- mapM readInterview participantFiles
  case sequence input of
    Left err -> putStrLn err
    Right xs -> print (makeInterview xs)
