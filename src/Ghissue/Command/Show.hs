module Ghissue.Command.Show (
  showCommand
) where

import Data.List(intercalate, sort)
import Ghissue
import Github.Auth(GithubAuth(..))
import Github.Data.Definitions(Issue
                              , IssueLabel
                              , issueNumber
                              , issueTitle
                              , issueLabels
                              , labelName)
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

{-
  This is the one-line summary of issues that we print when doing a full listing
-}
data LineSummary = LineSummary { lsNum :: Int
                               , lsTitle :: String
                               , lsLabels :: [String] }

instance Show LineSummary where
  show ls = let
    num = show (lsNum ls)
    title = lsTitle ls
    labs = intercalate ", " (lsLabels ls)
    in intercalate "\t" [num, title, labs]

toLineSummary :: Issue -> LineSummary
toLineSummary issue = let
  num = issueNumber issue
  title = issueTitle issue
  labs = sort $ map labelName (issueLabels issue)
  in LineSummary num title labs

printResults :: ShowArgs -> [Issue] -> IO ()
printResults sargs issues = do
  let lineSummaries = map toLineSummary issues
  let matching = filter (\ls -> listContains (lsNum ls) (showArgsIssues sargs)) lineSummaries
  sequence_ (map (putStrLn . show) matching)


showAction :: Config -> ShowArgs -> IO ()
showAction conf sargs = do
  let auth = GithubOAuth (configAuth conf)
  let org = configGithubOrg conf
  let repo = configRepo conf
  let limit = Labels (showArgsLabel sargs)
  eitherErrIssues <- issuesForRepo' (Just auth) org repo [limit]
  case eitherErrIssues of Left err -> fail (show err)
                          Right issues -> printResults sargs issues
