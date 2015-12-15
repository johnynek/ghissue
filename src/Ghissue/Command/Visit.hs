module Ghissue.Command.Visit (
  visitCommand
) where

import qualified Data.Text as T
import Ghissue
import Options.Applicative
import Turtle

visitCommand :: Command
visitCommand = Command { commandName = "visit"
                       , commandDesc = "Visit an issue in your default web browser"
                       , commandParser = visitParser
                       , commandAction = visitAction }

visitParser :: Parser Int
visitParser = argument auto (metavar "ISSUE" <> help "existing issue to visit on the web.")

{-| Create a shell command to open the url.
  This will need to be ported to something other than OSX
-}
openShell :: String -> String
openShell url = "open " ++ url

visitAction :: Config -> Int -> IO ()
visitAction conf issueNum = do
  let url = T.pack (openShell (urlForIssue conf issueNum))
  stdout (inshell url empty)


