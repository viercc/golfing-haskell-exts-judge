{-# LANGUAGE BlockArguments #-}
module Main where

import Data.List (delete, isPrefixOf)
import Data.Traversable (for)
import Control.Monad (unless)
import System.IO (hPutStrLn, hPutStr, stderr)
import System.Environment
import System.Exit

import System.Process
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

import Exts
import Source

main :: IO ()
main = checkEnv >> getArgs >>= parseArgs >>= body

expectedGhcVersion :: String
expectedGhcVersion = "8.10"

checkEnv :: IO ()
checkEnv =
  do ans <-
       readProcessWithExitCode "ghc" ["--numeric-version"] ""
     case ans of
       (ExitFailure e, _, err) -> hPutStrLn stderr err >> exitFailure
       (ExitSuccess, ans, _)
         | expectedGhcVersion `isPrefixOf` ans -> return ()
         | otherwise -> hPutStrLn stderr ("Unsupported GHC version:" ++ ans)
                        >> exitFailure

data Option = Option
  { extListFile :: FilePath
  , sourceFile :: FilePath
  , tmpBuildDir :: FilePath }

defaultOption :: Option
defaultOption = Option
  { extListFile = "extensions.txt"
  , sourceFile = "a.hs"
  , tmpBuildDir = "build"
  }

outputUsage :: IO ()
outputUsage =
  do putStrLn "judge --help | judge [<source>] [--exts <extlist>] [--build-dir <dir>]"
     putStrLn "\t<source>  (default=a.hs) Source file to be judged"
     putStrLn "\t<extlist> (default=extensions.txt) Extension list file"
     putStrLn "\t<dir>     (default=build) Directory which goes build artifacts"

parseArgs :: [String] -> IO Option
parseArgs = go defaultOption
  where
    go opt [] = return opt
    go opt ("--help":_)    = outputUsage >> exitSuccess
    go opt ("--exts":[])   = outputUsage >> exitFailure
    go opt ("--exts":a:as) = go opt{extListFile = a} as
    go opt ("--build-dir":[])   = outputUsage >> exitFailure
    go opt ("--build-dir":a:as) = go opt{tmpBuildDir = a} as
    go opt (a:as) | "-" `isPrefixOf` a = outputUsage >> exitFailure
                  | otherwise          = go opt{sourceFile=a} as

getRightIO :: Show e => Either e a -> IO a
getRightIO (Left e) = hPutStrLn stderr (show e) >> exitFailure
getRightIO (Right a) = return a

body :: Option -> IO ()
body opt =
  do (src, stat) <- readSourceFile (sourceFile opt)
     exts <- parseExtList (extListFile opt) >>= getRightIO
     let build = tmpBuildDir opt
         enabled = extEnabled exts
         implied = extImplied exts
     createDirectoryIfMissing True build

     writeSourceFile (build </> "all.hs") enabled [] src
     (passed, errMsg) <- compiles build (build </> "all.hs")
     unless passed
       do hPutStrLn stderr errMsg
          exitFailure
     
     resultsE <- for enabled $ \x ->
       do let file = build </> ("sans" ++ show x ++ ".hs")
          writeSourceFile file (delete x enabled) [] src
          (passedSans, _) <- compiles build file
          return [x | passedSans]
     resultsI <- for implied $ \x ->
       do let file = build </> ("sans" ++ show x ++ ".hs")
          writeSourceFile file enabled [x] src
          (passedSans, _) <- compiles build file
          return [x | passedSans]
     let unnecessary = concat (resultsE ++ resultsI)

     let numE  = length enabled
         numI  = length implied
         num   = numE + numI

         chars = srcLenCodepoints stat
         bytes = srcLenUtf8Bytes stat

         score :: Double
         score = fromIntegral chars / fromIntegral num
     
     if null unnecessary
       then do putStrLn "PASSED"
               putStrLn $ "  Score(chars/exts):" ++ show score
               putStrLn "================================="
               print stat
               putStrLn $ "exts=" ++ show num ++ "=" ++
                 show numE ++ "+" ++ show numI
       else do putStrLn "FAILED"
               putStrLn "  These extensions are not needed:"
               mapM_ print unnecessary
               exitFailure

compiles :: FilePath -> FilePath -> IO (Bool, String)
compiles dir file =
  do 
     (exitCode,_,err) <-
       readProcessWithExitCode "ghc"
         ["-main-is", "NoMain", "-c", "-outputdir", dir, "-O0", file] ""
     return (exitCode == ExitSuccess, err)
