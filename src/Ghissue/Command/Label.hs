module Ghissue.Command.Label (
  labelCommand
) where

import Control.Monad(sequence_)
import Data.List(union)
import Data.Either(either)
import Ghissue
import Github.Auth(GithubAuth(..))
import Github.Data.Definitions(EditIssue(..))
import Github.Issues
import Options.Applicative
{-
  ghissue new "some fool issue" --label confusing <<EOF ...
-}

labelCommand :: Command
labelCommand = Command { commandName = "label"
                       , commandDesc = "Label one or more issues"
                       , commandParser = labelParser
                       , commandAction = labelAction }

labels labs = EditIssue Nothing Nothing Nothing Nothing Nothing (Just labs)

zipA a b = (,) <$> a <*> b

labelParser :: Parser ([IssueRange], [String])
labelParser = let
  issues = argument issuesReadM (metavar "ISSUES" <> help "issues to label, e.g. 1-2,4-5,8")
  labels = some (strOption (long "label" <> short 'l' <> metavar "LABEL" <> help "labels to add"))
  in zipA issues labels

printLabel conf issue = do
  let isnum = issueNumber issue
  let url = urlForIssue conf isnum
  putStrLn ("labeled: " ++ (show isnum) ++ ", at: " ++ url)

failOnLeft :: (Show a, Monad m) => Either a b -> (b -> m c) -> m c
failOnLeft ethr fn = either (fail . show) fn ethr

labelAction :: Config -> ([IssueRange], [String]) -> IO ()
labelAction conf (irange, labs) = let
  auth = GithubOAuth (configAuth conf)
  org = configGithubOrg conf
  repo = configRepo conf
  ids = allIssues irange
  -- the new labels should be the union of old and new
  newLabels issue = union (map labelName (issueLabels issue)) labs
  setLabels issue = let
    issueN = issueNumber issue
    newLs = newLabels issue
    res = editIssue auth org repo issueN (labels newLs)
    print x = failOnLeft x (\_ -> putStrLn ((show issueN) ++ ". added: " ++ (show newLs)))
   in res >>= print
  updateLabs issues = let
    matching = filter (\ls -> listContains (issueNumber ls) irange) issues
    edits = map setLabels matching
    in sequence_ edits
  in do
  -- read current labels
  listResult <- issuesForRepo' (Just auth) org repo []
  issues <- failOnLeft listResult return
  updateLabs issues
