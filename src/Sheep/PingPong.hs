module Sheep.PingPong where

import Sheep.Common
import Sheep.Internal

import Control.Monad (void)
import Discord
import Discord.Requests qualified as R
import Discord.Types

herdFluffyPingPong :: Event -> Cache -> Maybe (DiscordHandler ())
herdFluffyPingPong (MessageCreate m) cache =
  if not (fromBot m) && messageAuthor m /= cacheCurrentUser cache && messageContent m == "ping"
    then pure $ void $ restCall (R.CreateMessage (messageChannelId m) "pong")
    else Nothing
herdFluffyPingPong _ _ = Nothing

slaughterFluffyPingPong :: IO ()
slaughterFluffyPingPong = pure ()

mkFluffyPingPong :: IO Sheep
mkFluffyPingPong = pure $ Sheep herdFluffyPingPong slaughterFluffyPingPong
