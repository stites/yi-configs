{-# LANGUAGE OverloadedStrings #-}
module MySnippets
    ( mySnippets
    ) where

import Data.Char (isUpper)
import Data.Monoid
import Control.Monad (void)
import System.FilePath
import qualified Data.Text as T

import qualified Yi.Rope as R
import Yi.Snippet

mySnippets :: [Snippet]
mySnippets
  =  derivingSnippets
  <> commentSnippets
  <> importSnippets
  <> pragmaSnippets
  <> codeSnippets

codeSnippets :: [Snippet]
codeSnippets =
  [ Snippet "m" $ do
      moduleName <- guessModuleName <$> refer filename
      line ("module " <> moduleName <> "    ) where")
  , Snippet "un" $ lit "undefined"
  , Snippet "(\\" $ void (lit "(\\ " *> place "_" <* lit " -> " <* place "undefined" <* lit ")" <* nl)
  , Snippet "if" $ void $ do
      lit "if "   *> place "" <* nl
      lit "then " *> place "" <* nl
      lit "else " *> place "" <* nl
  ]

pragmaSnippets :: [Snippet]
pragmaSnippets =
  -- language pragmas
  [ Snippet "pra"    $ void $ lit "{-# LANGUAGE " *> place "OverloadedStrings" <* lit " #-}" <* nl
  , Snippet "str"    $ line "{-# LANGUAGE OverloadedStrings #-}"
  , Snippet "gadt"   $ line "{-# LANGUAGE GADTs #-}"
  , Snippet "rank"   $ line "{-# LANGUAGE RankNTypes #-}"
  , Snippet "scoped" $ line "{-# LANGUAGE ScopedTypeVariables #-}"
  , Snippet "ffi"    $ line "{-# LANGUAGE ForeignFunctionInterface #-}"
  , Snippet "syn"    $ line "{-# LANGUAGE TypeSynonymInstances #-}"
  , Snippet "mparam" $ line "{-# LANGUAGE MultiParamTypeClasses #-}"
  , Snippet "bang"   $ line "{-# LANGUAGE BangPatterns #-}"
  , Snippet "sigs"   $ line "{-# LANGUAGE InstanceSigs #-}"
  , Snippet "mono"   $ line "{-# LANGUAGE NoMonomorphismRestriction #-}"
  , Snippet "stand"  $ line "{-# LANGUAGE StandaloneDeriving #-}"
  , Snippet "lambda" $ line "{-# LANGUAGE LambdaCase #-}"
  , Snippet "tuple"  $ line "{-# LANGUAGE TupleSections #-}"
  , Snippet "puns"   $ line "{-# LANGUAGE NamedFieldPuns #-}"
  , Snippet "temp"   $ line "{-# LANGUAGE TemplateHaskell #-}"
  , Snippet "flex"   $ do
      line "{-# LANGUAGE FlexibleInstances #-}"
      line "{-# LANGUAGE FlexibleContexts #-}"
      line "{-# LANGUAGE TypeSynonymInstances #-}"
  , Snippet "gnew"   $ do
      line "{-# LANGUAGE DeriveFunctor #-}"
      line "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
  , Snippet "constraints" $ do
      line "{-# LANGUAGE ConstraintKinds #-}"
      line "{-# LANGUAGE FlexibleContexts #-}"
  , Snippet "derive" $ do
      line "{-# LANGUAGE DeriveDataTypeable #-}"
      line "{-# LANGUAGE DeriveGeneric #-}"
      line "{-# LANGUAGE DeriveFunctor #-}"
      line "{-# LANGUAGE DeriveTraversable #-}"
      line "{-# LANGUAGE DeriveFoldable #-}"

  -- ghc-options
  , Snippet "opt"    $ line "{-# OPTIONS_GHC ${1} #-}"
  , Snippet "wall"   $ line "{-# OPTIONS_GHC -Wall #-}"
  , Snippet "nowarn" $ do
      line "{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}"
      line "{-# OPTIONS_GHC -fno-warn-type-defaults   #-}"
      line "{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}"
  ]

importSnippets :: [Snippet]
importSnippets =
  [ Snippet "im" $ void $ lit "import " *> place "Data.Text" <* nl
  , Snippet "iq" $ do
      lit "import qualified "
      moduleName <- place "Data.HashMap.Strict"
      lit " as "
      abbrev <- R.filter (`elem` ['A'..'Z']) . dropCommon <$> refer moduleName
      lit abbrev

  , Snippet "mm" $ do
      line "import Control.Monad"
      line "import Control.Applicative"
      line "import Control.Monad.State"
      line "import Control.Monad.Reader"
      line "import Control.Monad.Except"

  , Snippet "usual" $ do
      line "import Data.Maybe"
      line "import Data.Functor"
      line "import Data.Foldable"
      line "import Data.Traversable"
      line "import Control.Monad"
      line "import Control.Applicative"

  , Snippet "fold" $ do
      line "import Data.Foldable"
      line "import Data.Traversable"

  , Snippet "bs" $ do
      line "import qualified Data.ByteString as S"
      line "import qualified Data.ByteString.Char8 as S8"

  , Snippet "containers" $ do
      line "import qualified Data.HashMap.Lazy as HashMap"
      line "import qualified Data.HashSet      as HashSet"
      line "import qualified Data.IntMap       as IntMap"
      line "import qualified Data.IntSet       as IntSet"
      line "import qualified Data.IxSet        as IxSet"
      line "import qualified Data.Map          as Map"
      line "import qualified Data.Sequence     as Seq"
      line "import qualified Data.Set          as Set"
  ]

derivingSnippets :: [Snippet]
derivingSnippets =
  [ Snippet "dd" $ line "deriving (Eq, Ord, Show)"
  , Snippet "dg" $ line "deriving (Eq, Ord, Show, Typeable, Data, Generic)"
  ]

commentSnippets :: [Snippet]
commentSnippets =
  [ Snippet "--=" $ line "-- ========================================================================= --"
  , Snippet "---" $ line "-------------------------------------------------------------------------------"
  , Snippet "box"  $ genericbox "(c) Sam Stites, 2017" "BSD3"        "sam@stites.io"
  , Snippet "sbox" $ genericbox "(c) Sentenai, 2017"   "Proprietary" "sam@sentenai.com"
  ]
  where
    genericbox :: R.YiString -> R.YiString -> R.YiString -> SnippetBody ()
    genericbox c l m = do
      f <- guessModuleName <$> refer filename
      line "-------------------------------------------------------------------------------"
      line "-- |"
      lit  "-- Module    :  " *> place f <* nl
      lit  "-- Copyright :  " *> place c <* nl
      lit  "-- License   :  " *> place l <* nl
      lit  "-- Maintainer:  " *> place m <* nl
      line "-- Stability :  experimental"
      line "-- Portability: non-portable"
      line "--"
      lit  "-- " *> place "" <* nl
      line "-------------------------------------------------------------------------------"

guessModuleName :: R.YiString -> R.YiString
guessModuleName
  = R.fromText . T.intercalate "."
  . reverse . takeWhile isCapitalized . reverse
  . T.splitOn "/"
  . T.pack . dropExtension . T.unpack
  . R.toText
  where
    isCapitalized s = case T.uncons s of
      Just (c, _) -> isUpper c
      Nothing -> False

dropCommon :: R.YiString -> R.YiString
dropCommon s =
    case (R.split (== '.') s) of
        [x] -> x
        "Control" : rest -> R.intercalate "." rest
        "Data" : rest -> R.intercalate "." rest
        _ -> s


