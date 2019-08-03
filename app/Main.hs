{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Main where

import           Control.Monad        (when)
import qualified Data.ByteString.Lazy as B
import           Data.Foldable        (for_)
import           Data.List            (filter, isInfixOf)
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
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
    (Nothing, _)                -> putStrLn "No keywords given" <* exitFailure
    (Just [], _)                -> putStrLn "No keywords given" <* exitFailure
    (_, Nothing)                -> putStrLn "No amount of pages given" <* exitFailure
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
      let res = decodeUtf8 $ HTTP.getResponseBody r
      for_ keywords $ \keyword ->
        putStrLn $ (T.unpack $ T.intercalate "\n" $ highlightMatches keyword $ filterByKeyword keyword $ findTopics [] $ TS.parseTags res)
      printTopics (pageNum + 1) keywords pageLimit

findTopics :: [T.Text] -> [TS.Tag T.Text] -> [T.Text]
findTopics topics [] = topics
findTopics topics (TS.TagOpen ("a" :: T.Text) [("href", url), ("class", "topictitle")] : (TS.TagText topic) : rest) =
  findTopics (topics <> [topic]) rest
findTopics topics (_:rest) = findTopics topics rest

filterByKeyword :: String -> [T.Text] -> [T.Text]
filterByKeyword keyword topics =
  filter (isInfixOf keyword . T.unpack) topics

highlightMatches :: String -> [T.Text] -> [T.Text]
highlightMatches (T.pack -> keyword) matches =
  let highlightedKeyword = "\x1b[32m" <> keyword <> "\x1b[0m"
  in map (\t -> T.replace keyword highlightedKeyword t) matches
