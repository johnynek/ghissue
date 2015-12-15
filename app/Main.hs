module Main where

import Control.Monad(sequence_)
import Github.Auth
import Github.Issues
import Ghissue
import Ghissue.Command.Show(showCommand)
import Ghissue.Command.New(newCommand)

commands :: [Command]
commands = [showCommand, newCommand]

main :: IO ()
main = do
  conf <- readConfig
  toAction conf commands
