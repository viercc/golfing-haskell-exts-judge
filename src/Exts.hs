{-# LANGUAGE BlockArguments #-}
module Exts(
  parseExtList,
  ExtList(..),
  ParseError(..),
  ParseFailureCause(..),

  -- * Reexport
  KnownExtension(..)
) where

import Data.List (partition)
import Data.Either (partitionEithers)
import Data.Maybe (maybeToList)

import qualified Data.Set as Set

import System.IO
import Control.Exception

import Language.Haskell.Extension

data ExtList = ExtList
  { extEnabled :: [KnownExtension]
  , extImplied :: [KnownExtension] }
  deriving (Show, Eq)

data ParseError =
    ReadFail IOException
  | ParseFail [ParseFailureCause]
  deriving (Show)

data ParseFailureCause =
    Unknown String
  | Disabler KnownExtension
  | Deprecated KnownExtension [Extension]
  deriving (Show)

implies :: KnownExtension -> [KnownExtension]
implies DeriveTraversable = [DeriveFunctor, DeriveFoldable]
implies DerivingVia       = [DerivingStrategies]
implies FlexibleInstances = [TypeSynonymInstances]
implies FunctionalDependencies = [MultiParamTypeClasses]
implies GADTs                  = [GADTSyntax, MonoLocalBinds]
implies ImpredicativeTypes     = [RankNTypes]
implies IncoherentInstances    = [OverlappingInstances]
implies PolyKinds              = [KindSignatures]
implies RecordWildCards        = [DisambiguateRecordFields]
implies TypeFamilies           = [ExplicitNamespaces, KindSignatures, MonoLocalBinds]
implies TypeFamilyDependencies = [TypeFamilies] ++ implies TypeFamilies
implies TypeOperators          = [ExplicitNamespaces]
implies _                      = []

deprecation :: KnownExtension -> Maybe [Extension]
deprecation TypeInType =
  Just $ EnableExtension <$> [PolyKinds, DataKinds]
deprecation e = maybeToList <$> lookup (EnableExtension e) deprecatedExtensions

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile file =
  try do content <- readFile file
         _ <- evaluate (length content)
         return content

parseExtList :: FilePath -> IO (Either ParseError ExtList)
parseExtList file =
  either (Left . ReadFail) parseExtListStr <$> tryReadFile file

uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList

parseExtListStr :: String -> Either ParseError ExtList
parseExtListStr str
  | null errors = Right $ ExtList extIndep extDep
  | otherwise   = Left $ ParseFail errors
  where
    (errors, exts) = partitionEithers . fmap classifier . words $ str
    exts' = uniq exts
    impliedSet = Set.fromList $ exts' >>= implies
    (extDep, extIndep) = partition (`Set.member` impliedSet) exts'
    
    classifier extName = case classifyExtension extName of
      EnableExtension x ->
        case deprecation x of
          Nothing         -> Right x
          Just insteadUse -> Left $ Deprecated x insteadUse
      DisableExtension x -> Left $ Disabler x
      UnknownExtension x -> Left $ Unknown x
