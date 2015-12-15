module Ghissue.Command.Show (
  showCommand
) where

import Data.List(intercalate, sort)
import Data.Time.LocalTime(getCurrentTimeZone, utcToLocalTime)
import Ghissue
import Github.Auth(GithubAuth(..))
import Github.Data.Definitions(Issue
                              , IssueLabel
                              , issueNumber
                              , issueTitle
                              , issueLabels
                              , labelName
                              , GithubDate(fromGithubDate))
import Github.Issues
import Options.Applicative
{-
  ghissue show 1,3 --label foo
-}
data ShowArgs = ShowArgs { showArgsIssues :: [IssueRange]
                         , showArgsLabel :: [String]
                         }

showCommand :: Command
showCommand = Command { commandName = "show"
                      , commandDesc = "Show issues"
                      , commandParser = showParser
                      , commandAction = showAction }

showParser = let
  issueP = argument issuesReadM (metavar "ISSUES" <> help "issues to show, e.g. 1-2,4-5,8")
  labelP = strOption (long "label" <> short 'l' <> metavar "LABEL" <> help "labels which must be attached")
  in ShowArgs <$> (concat <$> (many issueP)) <*> (many labelP)

toLineSummary :: Issue -> IO String
toLineSummary issue = let
  num = show $ issueNumber issue
  title = issueTitle issue
  labs = intercalate "," (sort $ map labelName (issueLabels issue))
  commentCount = show (issueComments issue)
  updated = fromGithubDate (issueUpdatedAt issue)
  in do
    tz <- getCurrentTimeZone
    let local = show (utcToLocalTime tz updated)
    return (intercalate "\t" [num, title, local, commentCount, labs])

printResults :: ShowArgs -> [Issue] -> IO ()
printResults sargs issues = do
  let matching = filter (\ls -> listContains (issueNumber ls) (showArgsIssues sargs)) issues
  let lineSummaries = map toLineSummary matching
  sequence_ (map (\m -> m >>= putStrLn) lineSummaries)

showAction :: Config -> ShowArgs -> IO ()
showAction conf sargs = do
  let auth = GithubOAuth (configAuth conf)
  let org = configGithubOrg conf
  let repo = configRepo conf
  let limit = Labels (showArgsLabel sargs)
  eitherErrIssues <- issuesForRepo' (Just auth) org repo [limit]
  case eitherErrIssues of Left err -> fail (show err)
                          Right issues -> printResults sargs issues
