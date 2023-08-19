{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Sheep.Internal where

import Discord
import Discord.Types

{- |
`Sheep` represent independent services that can be interfaced via Collie-the-Bot.
-}
data Sheep = Sheep
  { herd :: Event -> Cache -> Maybe (DiscordHandler ())
  , slaughter :: IO ()
  }
