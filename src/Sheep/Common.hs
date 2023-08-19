module Sheep.Common where

import Data.Text (Text)
import Discord.Requests qualified as R
import Discord.Types

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

mentions :: User -> Message -> Bool
mentions user = elem (userId user) . fmap userId . messageMentions

reply :: Message -> Text -> R.MessageDetailedOpts
reply m t =
  R.MessageDetailedOpts
    t
    False
    Nothing
    Nothing
    Nothing
    (Just referenceMessage)
    Nothing
    Nothing
  where
    referenceMessage =
      MessageReference
        (Just $ messageId m)
        (Just $ messageChannelId m)
        (messageGuildId m)
        True
