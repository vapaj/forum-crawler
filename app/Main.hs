{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Main where

import           Control.Monad        (unless)
import qualified Data.ByteString.UTF8 as B
import           Data.Char            (toLower)
import           Data.Foldable        (for_)
import           Data.List            (filter, find, intercalate, isInfixOf,
                                       length, maximumBy, zip)
import           Data.Ord
import           Data.Tuple
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
      | Just (pages :: Int) <- readMay p ->
          for_ [digicamera, kameralaukku] $ \forum -> printTopics 1 keywords pages forum
      | otherwise -> putStrLn "No amount of pages given" <* exitFailure

digicamera :: Forum
digicamera =
  let forumName = "Digicamera.net"
      forumPageBaseUrl = "https://www.digicamera.net/keskus/viewforum.php?f=10"
      forumThreadBaseUrl = "https://www.digicamera.net/keskus"
      forumPaginationRule page =
        case page of
          1 -> forumPageBaseUrl
          n -> "https://www.digicamera.net/keskus/viewforum.php?f=10&start=" <> (show $ n * 25)
      forumThreadClassName = "topictitle"
  in Forum{..}

kameralaukku :: Forum
kameralaukku =
  let forumName = "Kameralaukku.com"
      forumPageBaseUrl = "https://foorumi.kameralaukku.com/forums/myydaeaen.9/"
      forumThreadBaseUrl = "https://foorumi.kameralaukku.com/"
      forumPaginationRule page =
        case page of
          1 -> forumPageBaseUrl
          n -> "https://foorumi.kameralaukku.com/forums/myydaeaen.9/page-" <> show n
      forumThreadClassName = "PreviewTooltip"
  in Forum{..}

printTopics :: Int -> [String] -> Int -> Forum -> IO ()
printTopics pageNum keywords pageLimit forum@Forum{..}
  | pageNum > pageLimit = pure ()
  | otherwise = do
      let url = forumPaginationRule pageNum
      initReq <- HTTP.parseRequest url
      r <- HTTP.httpBS initReq
      let res                = B.toString $ HTTP.getResponseBody r
          topics             = findTopics forum [] $ TS.parseTags res
          matches            = concatMap (filterByKeyword topics) keywords
          highlightedMatches = highlightAllMatches keywords $ map topicTitle matches
          matchesWithUrls    = map (topicToString . uncurry Topic) $ zip highlightedMatches $ map topicUrl matches
      unless (null matchesWithUrls)
        $ putStrLn $ intercalate "\n" matchesWithUrls
      printTopics (pageNum + 1) keywords pageLimit forum

data Forum = Forum
  { forumName            :: String
  , forumPageBaseUrl     :: String
  , forumThreadBaseUrl   :: String
  , forumPaginationRule  :: Int -> String
  , forumThreadClassName :: String
  }

data Topic = Topic
  { topicTitle :: String
  , topicUrl   :: String
  }

topicToString :: Topic -> String
topicToString Topic{topicTitle, topicUrl} = topicTitle ++ cyan " * " ++ topicUrl

green :: String -> String
green word = "\x1b[32m" ++ word ++ "\x1b[0m"

cyan :: String -> String
cyan word = "\x1b[36m" ++ word ++ "\x1b[0m"

findTopics :: Forum -> [Topic] -> [TS.Tag String] -> [Topic]
findTopics _ topics [] = topics
findTopics forum@Forum{..} topics (TS.TagOpen "a" attrs : (TS.TagText topic) : rest)
  | Just ("class", classValue) <- find (("class" ==) . fst) attrs
  , Just ("href", createThreadUrl forumThreadBaseUrl -> url) <- find (("href" ==) . fst) attrs
  , forumThreadClassName == classValue
  = findTopics forum (topics <> [Topic topic url]) rest
findTopics forum topics (_:rest) = findTopics forum topics rest

-- TODO: Use parser combinators
createThreadUrl :: String -> String -> String
createThreadUrl _baseUrl ('.' : url) = createThreadUrl _baseUrl url
createThreadUrl threadBaseUrl url    = threadBaseUrl ++ url

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
       then highlightMatchingWord keyword (final ++ green word) rest
       else highlightMatchingWord keyword (final ++ [w]) (ord ++ rest)

doesMatchCaseInsensitive :: String -> String -> Bool
doesMatchCaseInsensitive keyword matchCandidate = map toLower keyword == map toLower matchCandidate
