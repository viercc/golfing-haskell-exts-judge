{-# LANGUAGE OverloadedStrings #-}
module Source(
  Source,
  SourceStat(..),
  readSourceFile,
  writeSourceFile
) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Exts

type Source = T.Text
data SourceStat = SourceStat{
    srcLenCodepoints :: Int,
    srcLenUtf8Bytes :: Int
  }
  deriving (Show)

readSourceFile :: FilePath -> IO (Source, SourceStat)
readSourceFile file =
  do src <- BS.readFile file
     let srcT = T.decodeUtf8 src
         stat = SourceStat (T.length srcT) (BS.length src)
     return (srcT, stat)

buildOutput :: [KnownExtension] -> [KnownExtension] -> Source -> Source
buildOutput enables disables source = T.unlines $
     ["{-# LANGUAGE " <> T.pack (show ext)   <> "#-}" | ext <- enables ]
  ++ ["{-# LANGUAGE No" <> T.pack (show ext) <> "#-}" | ext <- disables ]
  ++ [ source ]

writeSourceFile
  :: FilePath
  -> [KnownExtension]
  -> [KnownExtension]
  -> Source
  -> IO ()
writeSourceFile file enables disables src =
  BS.writeFile file . T.encodeUtf8 $
    buildOutput enables disables src
