{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Main where

import qualified Data.ByteString.UTF8 as B
import           Data.Char            (toLower)
import           Data.Foldable        (for_)
import           Data.List            (filter, intercalate, isInfixOf)
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
      let res = B.toString $ HTTP.getResponseBody r
      for_ keywords $ \keyword ->
        putStrLn $ intercalate "\n"
        $ highlightMatches keyword
        $ filterByKeyword keyword
        $ findTopics [] $ TS.parseTags res
      printTopics (pageNum + 1) keywords pageLimit

findTopics :: [String] -> [TS.Tag String] -> [String]
findTopics topics [] = topics
findTopics topics (TS.TagOpen "a" [("href", _url), ("class", "topictitle")] : (TS.TagText topic) : rest) =
  findTopics (topics <> [topic]) rest
findTopics topics (_:rest) = findTopics topics rest

filterByKeyword :: String -> [String] -> [String]
filterByKeyword (map toLower -> keyword) topics =
  filter (isInfixOf keyword . map toLower) topics

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
