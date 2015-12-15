module Ghissue.Command.Close (
  closeCommand
) where

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
                       , commandDesc = "close an issue"
                       , commandParser = closeParser
                       , commandAction = closeAction }

closeParser :: Parser (Int, EditIssue)
closeParser = let
  issue = (argument auto (metavar "ISSUE") :: Parser Int)
  closed = EditIssue Nothing Nothing Nothing (Just "closed") Nothing Nothing
  in (\id -> (id, closed)) <$> issue

printClose issue = putStrLn (show issue)

closeAction :: Config -> (Int, EditIssue) -> IO ()
closeAction conf (id, ei) = do
  let auth = GithubOAuth (configAuth conf)
  let org = configGithubOrg conf
  let repo = configRepo conf
  eitherErrIssue <- editIssue auth org repo id ei
  case eitherErrIssue of Left err -> fail (show err)
                         Right issue -> printClose issue
