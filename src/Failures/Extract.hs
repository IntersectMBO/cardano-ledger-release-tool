{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Failures.Extract (extractCmd) where

import Cabal.Plan
import Common.Options (Options (..), options)
import Control.Monad (filterM, guard, unless)
import Data.Foldable (for_)
import Data.Strict.Either (partitionEithers)
import Data.Time (diffUTCTime, getCurrentTime)
import Failures.LogResults
import Failures.Parse
import Options.Applicative hiding (Failure)
import Say (sayErrString)
import System.Directory (doesDirectoryExist, doesFileExist, removePathForcibly)
import System.Exit (die)
import System.FilePath (makeRelative, takeDirectory, (<.>), (</>))
import UnliftIO.Async (pooledForConcurrently)

import qualified Data.Aeson as JSON
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data ExtractOptions = ExtractOptions
  { optProjectDir :: FilePath
  , optOutput :: FilePath
  }
  deriving (Show)

extractCmd :: Mod CommandFields (IO ())
extractCmd = do
  command "extract" $
    info
      ( helper <*> do
          optCommon <- options
          optProjectDir <-
            strOption $
              help "The project directory, or a subdirectory of it"
                <> short 'p'
                <> long "project"
                <> metavar "DIR"
                <> value "."
                <> showDefaultWith id
          optOutput <-
            strOption $
              help "Write output to FILE"
                <> short 'o'
                <> long "output"
                <> metavar "FILE"
                <> value "/dev/stdout"
                <> showDefaultWith id
          pure $ extract optCommon ExtractOptions {..}
      )
      (progDesc "Extract failure information from Cabal test logs")

extract :: Options -> ExtractOptions -> IO ()
extract Options {..} ExtractOptions {..} = do
  let
    trace n = if optVerbosity >= n then sayErrString else const mempty

  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist optProjectDir
    >>= (`unless` die ("Project directory " <> optProjectDir <> " doesn't exist"))

  root <-
    findProjectRoot optProjectDir
      >>= maybe (die $ "Can't find project root in " <> optProjectDir) pure

  trace 2 $ "Examining " <> root

  planLocation <- findPlanJson $ ProjectRelativeToDir root

  trace 2 $ "Plan location: " <> planLocation

  plan <- decodePlanJson planLocation

  suiteLogs <-
    filterM (doesFileExist . snd) $ do
      -- List monad
      unit <- Map.elems $ pjUnits plan
      guard $ uType unit == UnitTypeLocal
      Just dir <- [uDistDir unit]
      comp@(CompNameTest tName) <- Map.keys (uComps unit)
      let
        pId = uPId unit
        PkgId pName _ = pId
        PkgName name = pName
        suite = name <> ":" <> dispCompNameTarget pName comp
        -- `dir` will usually be absolute, in which case `(root </>)` is a no-op,
        -- but it's needed in testing because we have to use relative paths there
        file = root </> dir </> "test" </> T.unpack (dispPkgId pId <> "-" <> tName) <.> "log"
      pure (suite, file)

  trace 1 $ show (length suiteLogs) <> " logs found"

  let projectRoot = (takeDirectory . takeDirectory . takeDirectory) planLocation

  suiteRuns <-
    pooledForConcurrently suiteLogs $ \(suiteName, file) -> do
      let fileRelative = makeRelative projectRoot file
      (parseErrors, suiteFailures) <-
        partitionEithers <$> do
          trace 2 $ "Examining " <> fileRelative
          start <- getCurrentTime
          result <- parseLog fileRelative
          end <- getCurrentTime
          trace 2 $ "Finished  " <> fileRelative <> " (" <> show (end `diffUTCTime` start) <> ")"
          pure result
      for_ parseErrors $ \err -> do
        trace 0 $ "Warning: " <> show err <> " in " <> fileRelative
      pure SuiteRun {..}

  let PkgId _ (Ver logCompilerVersion) = pjCompilerId plan
      logSuiteRuns = filter (not . null . suiteFailures) suiteRuns

  if null logSuiteRuns
    then removePathForcibly optOutput
    else JSON.encodeFile optOutput LogResults {..}
