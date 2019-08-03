{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Main where

import           Control.Monad        (replicateM_)
import qualified Data.ByteString.Lazy as B
import           Data.List            (filter, isInfixOf)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import qualified Network.HTTP.Simple  as HTTP
import           System.Environment
import           Text.HTML.TagSoup    ((~==))
import qualified Text.HTML.TagSoup    as TS

main :: IO ()
main = do
  args <- getArgs
  let keyword = args !! 0
      pages   = args !! 1
  printTopics 1 keyword $ read pages

printTopics :: Int -> String -> Int -> IO ()
printTopics pageNum keyword pageLimit
  | pageNum > pageLimit = pure ()
  | otherwise = do
      let url 1 = "https://www.digicamera.net/keskus/viewforum.php?f=10"
          url n = "https://www.digicamera.net/keskus/viewforum.php?f=10&start=" <> (show $ n * 25)
      initReq <- HTTP.parseRequest $ url pageNum
      r <- HTTP.httpBS initReq
      let res = decodeUtf8 $ HTTP.getResponseBody r
      putStrLn $ (T.unpack $ T.intercalate "\n" $ highlightMatches keyword $ filterByKeyword keyword $ findTopics [] $ TS.parseTags res)
      printTopics (pageNum + 1) keyword pageLimit

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
