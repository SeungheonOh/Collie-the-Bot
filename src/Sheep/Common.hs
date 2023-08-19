module Sheep.Common where

import Data.Text (Text)
import Discord
import Discord.Requests qualified as R
import Discord.Types

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

mentions :: User -> Message -> Bool
mentions user = elem (userId user) . fmap userId . messageMentions

referenceMessage :: Message -> Bool -> MessageReference
referenceMessage msg =
  MessageReference
    (Just $ messageId msg)
    (Just $ messageChannelId msg)
    (messageGuildId msg)

reply :: Message -> Text -> R.MessageDetailedOpts
reply m t =
  R.MessageDetailedOpts
    t
    False
    Nothing
    Nothing
    Nothing
    (Just $ referenceMessage m True)
    Nothing
    Nothing
