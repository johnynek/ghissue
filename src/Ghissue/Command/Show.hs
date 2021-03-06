module Ghissue.Command.Show (
  showCommand
) where

import Data.List(intercalate, sort, transpose, length)
import Data.Time.LocalTime(getCurrentTimeZone, utcToLocalTime, TimeZone)
import Data.Maybe(fromMaybe)
import Ghissue
import Github.Auth(GithubAuth(..))
import Github.Data.Definitions(Issue
                              , IssueLabel
                              , IssueComment
                              , IssueComment(..)
                              , issueNumber
                              , issueTitle
                              , issueLabels
                              , labelName
                              , GithubDate(fromGithubDate)
                              , githubOwnerLogin)
import Github.Issues
import Github.Issues.Comments(comments')
import Options.Applicative
{-
  ghissue show 1,3 --label foo
-}
data ShowArgs = ShowArgs { showArgsIssues :: [IssueRange]
                         , showArgsLabel :: [String]
                         , showVerbose :: Bool
                         , showTabs :: Bool
                         }

showCommand :: Command
showCommand = Command { commandName = "show"
                      , commandDesc = "Show issues"
                      , commandParser = showParser
                      , commandAction = showAction }

showParser = let
  issueP = argument issuesReadM (metavar "ISSUES" <> help "issues to show, e.g. 1-2,4-5,8")
  labelP = strOption (long "label" <> short 'l' <> metavar "LABEL" <> help "labels which must be attached")
  verbose = switch (long "verbose" <> short 'v' <> help "Verbose: show full body and all comments")
  tabs = switch (long "tabs" <>
                 short 't' <>
                 help "Use tabs for columns. Useful with sort -t $'\t' -k3 | column -t -n $'\t'")
  in ShowArgs <$> (concat <$> (many issueP)) <*> (many labelP) <*> verbose <*> tabs

ownerName mgho = maybe "" githubOwnerLogin mgho


tabColumnarize :: [[String]] -> [String]
tabColumnarize = map (intercalate "\t")

{-| Pad the i^th string with enough space to make columns line up
  >>> evenColumnarize [["yo", "man"], ["foo", "bar"], ["bazbaz", "baby"]]
  ["yo     man ","foo    bar ","bazbaz baby"]
-}
evenColumnarize :: [[String]] -> [String]
evenColumnarize rows = let
  columns = transpose rows
  widths = map (\c -> maximum (map length c)) columns
  padTo t str = let
    sz = length str
    pads = t - sz
    tail = replicate pads ' '
    in str ++ tail
  resCol = map (\(w, c) -> map (padTo w) c) (zip widths columns)
  in map (intercalate " ") (transpose resCol)

glue2 :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
glue2 fn gn = \a b ->  gn (fn a b)

toLocalString :: TimeZone -> GithubDate -> String
toLocalString tz ghd = let
  updated = fromGithubDate ghd
  in show (utcToLocalTime tz updated)

toLineSummary :: TimeZone -> Issue -> [String]
toLineSummary tz issue = let
  num = show $ issueNumber issue
  title = issueTitle issue
  labs = intercalate "," (sort $ map labelName (issueLabels issue))
  commentCount = show (issueComments issue)
  local = toLocalString tz (issueUpdatedAt issue)
  assignee = ownerName (issueAssignee issue)
  in [num, title, local, commentCount, assignee, labs]

verboseStr :: TimeZone -> Issue -> String
verboseStr tz issue = let
  num = show $ issueNumber issue
  title = issueTitle issue
  labs = intercalate "," (sort $ map labelName (issueLabels issue))
  commentCount = show (issueComments issue)
  updated = fromGithubDate (issueUpdatedAt issue)
  local = show (utcToLocalTime tz updated)
  header = "#" ++ num ++ ": " ++ title ++ ", updated: " ++ local ++ "\n"
  body = fromMaybe "" (issueBody issue)
  in intercalate "\n" [header, body, ""]

commentsFor :: Config -> Issue -> IO [IssueComment]
commentsFor conf issue = do
  let auth = GithubOAuth (configAuth conf)
  let org = configGithubOrg conf
  let repo = configRepo conf
  let issueN = issueNumber issue
  result <- comments' (Just auth) org repo issueN
  failOnLeft result return

commentToStr :: TimeZone -> IssueComment -> String
commentToStr tz comment = let
  user = githubOwnerLogin (issueCommentUser comment)
  date = toLocalString tz (issueCommentUpdatedAt comment)
  body = issueCommentBody comment
  in user ++ " at " ++ date ++ "\n" ++ body ++ "\n\n"

issueWithComment :: Config -> TimeZone -> Issue -> IO String
issueWithComment conf tz issue = do
  let issueStr = verboseStr tz issue
  comments <- commentsFor conf issue
  let commentStrs = map (commentToStr tz) comments
  return (intercalate "\n" (issueStr : commentStrs))

printResults :: Config -> ShowArgs -> [Issue] -> IO ()
printResults conf sargs issues = do
  let matching = filter (\ls -> listContains (issueNumber ls) (showArgsIssues sargs)) issues
  tz <- getCurrentTimeZone
  let column = if (showTabs sargs) then tabColumnarize else evenColumnarize
  let verbFn = mapM (issueWithComment conf tz)
  let shortFn = return . column . map (toLineSummary tz)
  -- Here is a function: [Issue] -> IO [String]
  let showFn = if (showVerbose sargs) then verbFn else shortFn
  lineSummaries <- showFn matching
  sequence_ (map putStrLn lineSummaries)

showAction :: Config -> ShowArgs -> IO ()
showAction conf sargs = do
  let auth = GithubOAuth (configAuth conf)
  let org = configGithubOrg conf
  let repo = configRepo conf
  let limit = Labels (showArgsLabel sargs)
  eitherErrIssues <- issuesForRepo' (Just auth) org repo [limit]
  failOnLeft eitherErrIssues (printResults conf sargs)
