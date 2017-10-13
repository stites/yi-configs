{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
import Control.Monad.State.Lazy (execStateT)
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=))
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))
import Control.Monad (when, unless)

import Options.Applicative

import Yi hiding (option, super)
import Yi.Config.Simple.Types
import Yi.Buffer.Misc (lineMoveRel)
import qualified Yi.Snippet as Snippet

import Yi.Config (Config(debugMode))
import Yi.Config.Default.HaskellMode    (configureHaskellMode)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes      (configureMiscModes)

import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V
import qualified Yi.Keymap.Vim.Utils as V
import qualified Yi.Frontend.Vty as Vty

import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
import Yi.Debug (initDebug)

import qualified Make
import Yi.Intero
import Yi.Fuzzy
import qualified Data.Attoparsec.Text as P
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import Data.Text (Text,unpack)
import Yi.Tag (tagsFileList)

import MySnippets (mySnippets)
import MyIntero (interoExCommands, exInteroEval, parseText, exInteroStart, exInteroLocAt, exInteroUses, exInteroTypeAt)
import Yi.Ghcid (ghcid)

data CommandLineOptions = CommandLineOptions
  { debug :: Bool
  , startOnLine :: Maybe Int
  , files :: [String]
  }

commandLineOptions :: Parser (Maybe CommandLineOptions)
commandLineOptions = nada <|> yada
  where
    nada :: Parser (Maybe CommandLineOptions)
    nada = flag' Nothing
      ( long "version"
      <> short 'v'
      <> help "Show the version number")

    yada :: Parser (Maybe CommandLineOptions)
    yada = Just <$> (CommandLineOptions
      <$> (flag False True
          ( long "debug"
         <> help "log output to yi-debug-output.txt"))
      <*> optional (option auto
          ( long "line"
         <> short 'l'
         <> metavar "NUM"
         <> help "Open the (last) file on line NUM"))
      <*> many (argument str (metavar "FILES...")))

main :: IO ()
main =
  execParser opts >>= \case
    Nothing -> putStrLn "Yi 0.16.0"
    Just clo -> do
      let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
          moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine clo)))
      config :: Config <- execStateT
          (runConfigM myConfig >> (startActionsA .= (openFileActions ++ [moveLineAction])))
          defaultConfig
      when (debug clo) (initDebug "yi-debug-output.txt")
      startEditor config Nothing
  where
   opts = info (helper <*> commandLineOptions)
     ( fullDesc
    <> progDesc "Edit files"
    <> header "Yi - a flexible and extensible text editor written in haskell")

myConfig :: ConfigM ()
myConfig = do
  configureVim
  configureVty
  configureHaskellMode
  configureJavaScriptMode
  configureMiscModes
  defaultKmA .= myKeymapSet
  tagsFileList .= ["tags", "codex.tags"]

myKeymapSet :: KeymapSet
myKeymapSet = V.mkKeymapSet $ V.defVimConfig `override` custom
  where
    custom :: V.VimConfig -> V.VimConfig -> V.VimConfig
    custom old new = old
      { V.vimBindings = myBindings eval ++ V.vimBindings old
      , V.vimExCommandParsers = interoExCommands <> V.vimExCommandParsers old
      }
      where
        eval :: V.EventString -> EditorM ()
        eval = V.pureEval new


myBindings :: (V.EventString -> EditorM ()) -> [V.VimBinding]
myBindings eval =
  [ nmap  "<BS>"   previousTabE
  , nmap  "<Tab>"  nextTabE
  , nmap  " "      (eval ":nohlsearch<CR>")
  , nmap  ";"      (eval ":")
  , nmapY "<C-p>"  fuzzyOpen
  , nmapY "<C-g>"  ghcid
  , nmap  "<C-s>"  (withCurrentBuffer deleteTrailingSpaceB)
  , nmap  "<C-d>"  (withCurrentBuffer $ scrollScreensB   1 )
  , nmap  "<C-u>"  (withCurrentBuffer $ scrollScreensB (-1))
  , nmap  "<C-S-l>"  (withCurrentBuffer (transposeB unitWord Forward >> leftB))
  , nmap  "<C-S-h>"  (withCurrentBuffer (transposeB unitWord Backward))
  , nmap  "<C-@>"  Make.showErrorE
  , nmap  "<M-d>"  Make.debug
  , nmap   ",s"    Make.insertErrorMessageE
  , imapY  "<Tab>" (withEditor expander)
  ]
  where
    expander = do
      expanded <- Snippet.expandSnippetE (defEval "<Esc>") mySnippets
      unless expanded (defEval "  ")

    defEval   = V.pureEval (extractValue V.defVimConfig)
    nmap  x y = V.mkStringBindingE V.Normal V.Drop (x, y, id)
    nmapY x y = V.mkStringBindingY V.Normal (x, y, id)
    vmapY x y = V.mkStringBindingY (V.Visual LineWise) (x, y, id)
    imapY x y = V.VimBindingY $ \evs state ->
        case V.vsMode state of
            V.Insert _ -> fmap (const (y >> return V.Drop)) (evs `V.matchesString` x)
            _ -> V.NoMatch


