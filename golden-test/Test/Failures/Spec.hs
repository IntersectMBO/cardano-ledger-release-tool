module Test.Failures.Spec where

import System.Exit (ExitCode (..))
import System.FilePath.Find (always, fileName, find, (==?))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcessWithExitCode)
import Test.Common.Fixture (fixturePath)
import Test.Common.Golden (goldenTest)
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as T

spec :: Spec
spec = do
  it "extract" $ do
    withSystemTempFile "extract.actual" $ \actualFailures h -> do
      hClose h
      logsDir <- fixturePath "Failures/extract"
      expectedFailures <- fixturePath "Failures/extract-failures.golden"
      expectedErr <- fixturePath "Failures/extract-err.golden"
      (code, out, err) <- readProcessWithExitCode "cleret" ["failures", "extract", "-o", actualFailures, "-p", logsDir] ""
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` null
      goldenTest expectedErr $ T.pack err
      goldenTest expectedFailures =<< T.readFile actualFailures
  it "render" $ do
    withSystemTempFile "render.actual" $ \actual h -> do
      hClose h
      failuresDir <- fixturePath "Failures/render"
      expected <- fixturePath "Failures/render.golden"
      failuresFiles <- find always (fileName ==? "failures.json") failuresDir
      callProcess "cleret" $ ["failures", "render", "-o", actual] <> failuresFiles
      goldenTest expected =<< T.readFile actual
