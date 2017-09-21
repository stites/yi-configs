{-# LANGUAGE OverloadedStrings #-}
module MyIntero
  ( interoExCommands
  , exInteroEval
  , parseText
  , exInteroStart
  , exInteroLocAt
  , exInteroUses
  , exInteroTypeAt
  ) where


import Control.Monad.State hiding (state)
import Data.List (intersperse)
import Data.Monoid
import qualified Data.Text as T
import Lens.Micro.Platform
import System.Directory (getCurrentDirectory)
import System.Environment


import Yi (Action)
import Yi.Intero (interoEval, interoStart, interoTypeAt, interoUses, interoLocAt)
import qualified Data.Attoparsec.Text as P
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import Data.Text (Text,unpack)

import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V
import qualified Yi.Keymap.Vim.Utils as V

interoExCommands :: [V.EventString -> Maybe V.ExCommand]
interoExCommands = [exInteroUses,exInteroTypeAt,exInteroEval,exInteroLocAt,exInteroStart]

exInteroEval :: V.EventString -> Maybe V.ExCommand
exInteroEval = Common.parse $ do
  void $ P.string "intero-eval"
  void $ P.many1 P.space
  instr <- P.takeWhile (const True)
  return $ Common.impureExCommand
    { V.cmdShow = "intero-eval " <> instr
    , V.cmdAction = interoEval (unpack instr)
    }

parseText :: Text -> Action -> V.EventString -> Maybe V.ExCommand
parseText txt action = Common.parse $ do
  void $ P.string txt
  return $ Common.impureExCommand
    { V.cmdShow = txt
    , V.cmdAction = action
    }

exInteroStart, exInteroLocAt,exInteroUses,exInteroTypeAt :: V.EventString -> Maybe V.ExCommand
exInteroStart  = parseText "intero-start"   interoStart
exInteroLocAt  = parseText "intero-loc-at"  interoLocAt
exInteroUses   = parseText "intero-uses"    interoUses
exInteroTypeAt = parseText "intero-type-at" interoTypeAt


