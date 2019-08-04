{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Main where

import qualified Data.ByteString.UTF8 as B
import           Data.Char            (toLower)
import           Data.Foldable        (for_)
import           Data.List            (filter, intercalate, isInfixOf, length,
                                       maximumBy, zip)
import           Data.Ord
import qualified Network.HTTP.Simple  as HTTP
import           Safe                 (initMay, lastMay, readMay)
import           System.Environment
import           System.Exit
import           Text.HTML.TagSoup    ((~==))
import qualified Text.HTML.TagSoup    as TS

main :: IO ()
main = do
  args <- getArgs
  let maybeKeywords   = Safe.initMay args
      maybeNumOfPages = Safe.lastMay args
  case (maybeKeywords, maybeNumOfPages) of
    (Nothing, _) -> putStrLn "No keywords given" <* exitFailure
    (Just [], _) -> putStrLn "No keywords given" <* exitFailure
    (Just keywords, Just p)
      | Just (pages :: Int) <- readMay p -> printTopics 1 keywords pages
      | otherwise -> putStrLn "No amount of pages given" <* exitFailure

printTopics :: Int -> [String] -> Int -> IO ()
printTopics pageNum keywords pageLimit
  | pageNum > pageLimit = pure ()
  | otherwise = do
      let url 1 = "https://www.digicamera.net/keskus/viewforum.php?f=10"
          url n = "https://www.digicamera.net/keskus/viewforum.php?f=10&start=" <> (show $ n * 25)
      initReq <- HTTP.parseRequest $ url pageNum
      r <- HTTP.httpBS initReq
      let res                = B.toString $ HTTP.getResponseBody r
          topics             = findTopics [] $ TS.parseTags res
          matches            = concatMap (filterByKeyword topics) keywords
          highlightedMatches = highlightAllMatches keywords $ map topicTitle matches
          matchesWithUrls = map (topicToString . uncurry Topic) $ zip highlightedMatches $ map topicUrl matches
      putStrLn $ intercalate "\n" matchesWithUrls
      printTopics (pageNum + 1) keywords pageLimit

threadUrlBase :: String
threadUrlBase = "https://www.digicamera.net/keskus"

data Topic = Topic
  { topicTitle :: String
  , topicUrl   :: String
  }

topicToString :: Topic -> String
topicToString Topic{topicTitle, topicUrl} = topicTitle ++ " -> " ++ topicUrl

findTopics :: [Topic] -> [TS.Tag String] -> [Topic]
findTopics topics [] = topics
findTopics topics (TS.TagOpen "a" [("href", ('.' : url)), ("class", "topictitle")] : (TS.TagText topic) : rest) =
  findTopics (topics <> [Topic topic $ threadUrlBase ++ url]) rest
findTopics topics (_:rest) = findTopics topics rest

filterByKeyword :: [Topic] -> String -> [Topic]
filterByKeyword topics (map toLower -> keyword) =
  filter (isInfixOf keyword . map toLower . topicTitle) topics

highlightAllMatches :: [String] -> [String] -> [String]
highlightAllMatches [] highlightedMatches = highlightedMatches
highlightAllMatches (keyword : rest) matches =
  highlightAllMatches rest $ highlightMatches keyword matches

highlightMatches :: String -> [String] -> [String]
highlightMatches keyword matches =
 map (highlightMatchingWord keyword "") matches

highlightMatchingWord :: String -> String -> String -> String
highlightMatchingWord _ final "" = final
highlightMatchingWord keyword final matchingSentence =
  let l = length keyword
      (word@(w : ord), rest) = splitAt l matchingSentence
  in if doesMatchCaseInsensitive keyword word
       then highlightMatchingWord keyword (final ++ "\x1b[32m" ++ word ++ "\x1b[0m") rest
       else highlightMatchingWord keyword (final ++ [w]) (ord ++ rest)

doesMatchCaseInsensitive :: String -> String -> Bool
doesMatchCaseInsensitive keyword matchCandidate = map toLower keyword == map toLower matchCandidate
