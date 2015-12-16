module Ghissue.Command.All(allCommands) where

import Ghissue(Command)
import Ghissue.Command.Assign
import Ghissue.Command.Close
import Ghissue.Command.Comment
import Ghissue.Command.Label
import Ghissue.Command.New
import Ghissue.Command.Show
import Ghissue.Command.Visit

allCommands :: [Command]
allCommands = [showCommand
              , newCommand
              , closeCommand
              , labelCommand
              , visitCommand
              , commentCommand
              , assignCommand]
