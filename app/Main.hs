module Main where

import Control.Monad(sequence_)
import Github.Auth
import Github.Issues
import Ghissue
import Ghissue.Command.Show(showCommand)

commands :: [Command]
commands = [showCommand]

main :: IO ()
main = do
  conf <- readConfig
  toAction conf commands
