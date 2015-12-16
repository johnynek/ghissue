module Ghissue.Command.Comment (
  commentCommand
) where

import qualified Data.Text as T
import Ghissue
import Github.Auth(GithubAuth(..))
import Github.Issues.Comments(createComment)
import Options.Applicative

commentCommand :: Command
commentCommand = Command { commandName = "comment"
                         , commandDesc = "Attach a comment to a given issue. If no --body is given stdin is used"
                         , commandParser = commentParser
                         , commandAction = commentAction }

data Comment = Comment { commentIssueNumber :: Int
                       , commentBody :: Maybe String
                       }

commentParser :: Parser Comment
commentParser = let
  issue = argument auto (metavar "ISSUE" <> help "existing issue to comment on the web.")
  body = optional (strOption (long "body" <>
                              short 'b' <>
                              metavar "BODY" <>
                              help "if BODY is absent it is read from stdin"))
  in Comment <$> issue <*> body

withBody :: Comment -> IO String
withBody c@(Comment { commentBody = Just body }) = return body
withBody _ = do getContents

commentAction :: Config -> Comment -> IO ()
commentAction conf com = do
  let auth = GithubOAuth (configAuth conf)
  let org = configGithubOrg conf
  let repo = configRepo conf
  let issueN = commentIssueNumber com
  body <- withBody com
  res <- createComment auth org repo issueN body
  failOnLeft res (\_ -> return ())


