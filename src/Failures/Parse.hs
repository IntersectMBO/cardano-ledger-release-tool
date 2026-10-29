{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Failures.Parse (parseLog) where

import Control.Exception (evaluate)
import Data.Maybe (mapMaybe)
import Data.Strict.Either (Either (..), partitionEithers)
import Data.Text (Text)
import Failures.LogResults
import GHC.Generics (Generic)
import Text.Regex.Applicative
import Prelude hiding (Either (..))

import qualified Data.Text as T
import qualified Data.Text.IO as T

data ParseError
  = OrphanedSelector !Option
  | OrphanedSeed !Option
  deriving (Eq, Ord, Show, Generic)

parseLog :: FilePath -> IO ([ParseError], [Failure])
parseLog fp = do
  infos <- mapMaybe findInfo . T.lines <$> T.readFile fp
  evaluate $ collectFailures infos

collectFailures :: [ReproInfo] -> ([ParseError], [Failure])
collectFailures = partitionEithers . go
 where
  go (Seed seed : Selector sel : infs) =
    Right (Failure sel seed) `cons` go infs
  go (Selector sel : Seed seed : infs) =
    Right (Failure sel seed) `cons` go infs
  go (SelectorAndSeed sel seed : infs) =
    Right (Failure sel seed) `cons` go infs
  go (Selector sel : infs) =
    Left (OrphanedSelector sel) `cons` go infs
  go (Seed seed : infs) =
    Left (OrphanedSeed seed) `cons` go infs
  go [] = []
  -- We need the result to be strict because we may be parsing a large number of large files
  -- and laziness would retain the `Text` content of the files instead of just the summaries.
  !x `cons` !xs = let !ys = x : xs in ys

data ReproInfo
  = Selector !Option
  | Seed !Option
  | SelectorAndSeed !Option !Option
  deriving (Eq, Ord, Show)

findInfo :: Text -> Maybe ReproInfo
findInfo = fmap fst . findLongestPrefixWithUncons T.uncons (few anySym *> reproInfo)

reproInfo :: RE Char ReproInfo
reproInfo =
  asum
    [ Selector <$ "Use " <*> (option "-p" <* " '" <*> text <* "'")
    , Seed <$ "Use " <*> (option "--quickcheck-replay" <* "=\"" <*> text <* "\"")
    , SelectorAndSeed
        <$ "To rerun use: "
        <*> (option "--match" <* " \"" <*> text <* "\" ")
        <*> (option "--seed" <* " " <*> text)
    , SelectorAndSeed
        <$ "Use \""
        <*> (option "--pattern" <* " '" <*> text <* "' ")
        <*> (option "--hedgehog-replay" <* " '" <*> text <* "'\"")
    ]
 where
  option name = Option . T.pack <$> string name
  text = T.pack <$> few anySym
