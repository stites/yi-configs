{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State.Lazy (execStateT)
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=))
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))
import Control.Monad (when)

import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V

import Options.Applicative

import Yi hiding (option, super)
import Yi.Config.Simple.Types
import Yi.Buffer.Misc (lineMoveRel)
import qualified Yi.Snippet as Snippet

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

import Make
import Yi.Intero
import Yi.Fuzzy
import qualified Data.Attoparsec.Text as P
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import Data.Text (Text,unpack)

import MySnippets (mySnippets)
import MyIntero (interoExCommands, exInteroEval, parseText, exInteroStart, exInteroLocAt, exInteroUses, exInteroTypeAt)

data CommandLineOptions = CommandLineOptions
  { startOnLine :: Maybe Int
  , files :: [String]
  }

commandLineOptions :: Parser (Maybe CommandLineOptions)
commandLineOptions = flag' Nothing
                       ( long "version"
                      <> short 'v'
                      <> help "Show the version number")
  <|> (Just <$> (CommandLineOptions
    <$> optional (option auto
        ( long "line"
       <> short 'l'
       <> metavar "NUM"
       <> help "Open the (last) file on line NUM"))
    <*> many (argument str (metavar "FILES..."))
  ))

main :: IO ()
main = do
    mayClo <- execParser opts
    case mayClo of
      Nothing -> putStrLn "Yi 0.16.0"
      Just clo -> do
        let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
            moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine clo)))
        cfg <- execStateT
            (runConfigM myConfig >> (startActionsA .= (openFileActions ++ [moveLineAction])))
            defaultConfig
        startEditor cfg Nothing
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


myKeymapSet :: KeymapSet
myKeymapSet = V.mkKeymapSet $ V.defVimConfig `override` \super this ->
    let eval = V.pureEval this
    in super
        { V.vimBindings = myBindings eval ++ V.vimBindings super
        , V.vimExCommandParsers = interoExCommands <> V.vimExCommandParsers super
        }


myBindings :: (V.EventString -> EditorM ()) -> [V.VimBinding]
myBindings eval =
  [ nmap  "<BS>"   previousTabE
  , nmap  "<Tab>"  nextTabE
  , nmap  " "      (eval ":nohlsearch<CR>")
  , nmap  ";"      (eval ":")
  , nmapY "<C-p>"  fuzzyOpen
  , nmap  "<C-s>"  (withCurrentBuffer deleteTrailingSpaceB)
  , nmap  "<C-d>"  (withCurrentBuffer $ scrollScreensB   1 )
  , nmap  "<C-u>"  (withCurrentBuffer $ scrollScreensB (-1))
  , nmap  "<C-S-l>"  (withCurrentBuffer (transposeB unitWord Forward >> leftB))
  , nmap  "<C-S-h>"  (withCurrentBuffer (transposeB unitWord Backward))
  , nmap  "<C-@>"  showErrorE
  , nmap  "<M-d>"  debug
  , nmap   ",s"    insertErrorMessageE
  , imapY  "<Tab>" (withEditor expander)
  ]
  where
    expander = do
      expanded <- Snippet.expandSnippetE (defEval "<Esc>") mySnippets
      when (not expanded) (defEval "  ")

    defEval   = V.pureEval (extractValue V.defVimConfig)
    nmap  x y = V.mkStringBindingE V.Normal V.Drop (x, y, id)
    nmapY x y = V.mkStringBindingY V.Normal (x, y, id)
    vmapY x y = V.mkStringBindingY (V.Visual LineWise) (x, y, id)
    imapY x y = V.VimBindingY $ \evs state ->
        case V.vsMode state of
            V.Insert _ -> fmap (const (y >> return V.Drop)) (evs `V.matchesString` x)
            _ -> V.NoMatch


