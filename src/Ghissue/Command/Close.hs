module Ghissue.Command.Close (
  closeCommand
) where

import Control.Monad(sequence_)
import Ghissue
import Github.Auth(GithubAuth(..))
import Github.Data.Definitions(EditIssue(..))
import Github.Issues
import Options.Applicative
{-
  ghissue new "some fool issue" --label confusing <<EOF ...
-}

closeCommand :: Command
closeCommand = Command { commandName = "close"
                       , commandDesc = "Close one or more issues"
                       , commandParser = closeParser
                       , commandAction = closeAction }

closed = EditIssue Nothing Nothing Nothing (Just "closed") Nothing Nothing

closeParser :: Parser [Int]
closeParser = let
  issues = argument issuesReadM (metavar "ISSUES" <> help "issues to close, e.g. 1-2,4-5,8")
  in allIssues <$> issues

printClose conf issue = do
  let isnum = issueNumber issue
  let url = urlForIssue conf isnum
  putStrLn ("closed: " ++ (show isnum) ++ ", at: " ++ url)

closeAction :: Config -> [Int] -> IO ()
closeAction conf ids = let
  auth = GithubOAuth (configAuth conf)
  org = configGithubOrg conf
  repo = configRepo conf
  close id = do
    eitherErrIssue <- editIssue auth org repo id closed
    case eitherErrIssue of Left err -> fail (show err)
                           Right issue -> printClose conf issue
  in sequence_ (map close ids)
