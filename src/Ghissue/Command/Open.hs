module Ghissue.Command.Open (
  openCommand
) where

import qualified Data.Text as T
import Ghissue
import Options.Applicative
import Turtle

openCommand :: Command
openCommand = Command { commandName = "open"
                      , commandDesc = "Open an issue in your default web browser"
                      , commandParser = openParser
                      , commandAction = openAction }

openParser :: Parser Int
openParser = argument auto (metavar "ISSUE" <> help "issue to open")

{-| Create a shell command to open the url.
  This will need to be ported to something other than OSX
-}
openShell :: String -> String
openShell url = "open " ++ url

openAction :: Config -> Int -> IO ()
openAction conf issueNum = do
  let url = T.pack (openShell (urlForIssue conf issueNum))
  stdout (inshell url empty)


