module Ghissue.Command.Assign (
  assignCommand
) where

import Control.Monad(sequence_)
import Data.List(union, (\\))
import Data.Either(either)
import Ghissue
import Github.Auth(GithubAuth(..))
import Github.Data.Definitions(EditIssue(..))
import Github.Issues
import Options.Applicative
{-
  ghissue assign 12 johnynek
-}

assignCommand :: Command
assignCommand = Command { commandName = "assign"
                        , commandDesc = "Assign one or more issues to a given user"
                        , commandParser = assignParser
                        , commandAction = assignAction }

assigns maybeUser = EditIssue Nothing Nothing maybeUser Nothing Nothing Nothing

data AssignArg = AssignArg { assignRange :: [IssueRange]
                           , assignAssigns :: Maybe String
                           }

assignParser :: Parser AssignArg
assignParser = let
  issues = argument issuesReadM (metavar "ISSUES" <> help "issues to assign, e.g. 1-2,4-5,8")
  assignee = optional (argument str (metavar "ASSIGNEE" <> help "assign to ASSIGNEE. Missing means unassign"))
  in AssignArg <$> issues <*> assignee

printAssign conf issue = do
  let isnum = issueNumber issue
  let url = urlForIssue conf isnum
  putStrLn ("assigned: " ++ (show isnum) ++ ", at: " ++ url)


assignAction :: Config -> AssignArg -> IO ()
assignAction conf AssignArg { assignRange = irange, assignAssigns = assignee} = let
  auth = GithubOAuth (configAuth conf)
  org = configGithubOrg conf
  repo = configRepo conf
  ids = allIssues irange
  setAssign issueN = do
    let message = case assignee of Just name -> " assigned: " ++ name
                                   Nothing -> " unassigned."
    res <- editIssue auth org repo issueN (assigns assignee)
    failOnLeft res (\_ -> putStrLn ((show issueN) ++ message))
  in fmap (\_ -> ()) (mapM setAssign ids)
