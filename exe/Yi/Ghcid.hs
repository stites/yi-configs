{-# LANGUAGE OverloadedStrings #-}

module Yi.Ghcid (ghcid) where

import Control.Monad (void)
import Control.Monad.State (gets)
import Data.Binary (Binary(..), Word8)
import Data.Default (Default(..))
import Data.List (isSuffixOf)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import GHC.Exts (IsList(..))

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M

import Yi
import Yi.Completion
import Yi.MiniBuffer
import Yi.Types
import Yi.Utils ()
import qualified Yi.Rope as R

import qualified Language.Haskell.Ghcid as Ghcid


-- | Fuzzy-opens the directory to the specified depth. The depth needs
-- to be at least @1@ for it to do anything meaningful.
ghcid :: YiM ()
ghcid = do
  withMinibufferFree "Ghcid: " (\t -> pure ())

