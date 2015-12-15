module Ghissue.Command.New (
  newCommand
) where

import Ghissue
import Github.Auth(GithubAuth(..))
import Github.Data.Definitions(NewIssue(..))
import Github.Issues
import Options.Applicative
{-
  ghissue new "some fool issue" --label confusing <<EOF ...
-}

newCommand :: Command
newCommand = Command { commandName = "new"
                      , commandDesc = "create a new issue"
                      , commandParser = newParser
                      , commandAction = newAction }

maybeNonEmpty :: [a] -> Maybe [a]
maybeNonEmpty [] = Nothing
maybeNonEmpty as = Just as

newParser = let
  title = argument str (metavar "TITLE")
  body = optional (strOption (long "body" <>
                              short 'b' <>
                              metavar "BODY" <>
                              help "if BODY is absent it is read from stdin"))
  assignee = optional (strOption (long "assign" <>
                              short 'a' <>
                              metavar "ASSIGNEE" <>
                              help "github user ID to assign to the issue"))
  milestone = pure Nothing -- todo
  labels = Just <$> (many (strOption (long "label" <>
                                               short 'l' <>
                                               metavar "LABEL" <>
                                               help "attach LABEL to the issue")))
  in NewIssue <$> title <*> body <*> assignee <*> milestone <*> labels

withBody :: NewIssue -> IO NewIssue
withBody ni@(NewIssue { newIssueBody = Just body }) = return ni
withBody ni = do
  body <- getContents
  return (ni { newIssueBody = Just body })

printIssue conf issue = do
  let isnum = issueNumber issue
  let url = urlForIssue conf isnum
  putStrLn ("created: " ++ (show isnum) ++ ", at: " ++ url)

newAction :: Config -> NewIssue -> IO ()
newAction conf ni = do
  let auth = GithubOAuth (configAuth conf)
  let org = configGithubOrg conf
  let repo = configRepo conf
  nibody <- withBody ni
  eitherErrIssues <- createIssue auth org repo nibody
  case eitherErrIssues of Left err -> fail (show err)
                          Right issue -> printIssue conf issue
