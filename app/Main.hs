{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Sheep.GHCI
import Sheep.Internal
import Sheep.PingPong

import Control.Applicative (asum)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import System.Environment (lookupEnv)

import Discord
import Discord.Types

onStart :: DiscordHandler ()
onStart = pure ()

onEnd :: [Sheep] -> IO ()
onEnd = mapM_ slaughter

onEvent :: [Sheep] -> Event -> DiscordHandler ()
onEvent sheeps e = do
  cache <- readCache
  let action = asum $ ($ cache) . ($ e) . herd <$> sheeps
  lift $ print e

  case action of
    Nothing -> do
      lift $ putStrLn "No sheep"
      pure ()
    Just a -> a

main :: IO ()
main = do
  token <-
    T.pack . fromMaybe (error "Provide Discord API token via DISCORD_TOKEN envar")
      <$> lookupEnv "DISCORD_TOKEN"

  sheeps <-
    sequenceA
      [ mkFluffyPingPong
      , mkFluffyGHCI
      ]

  err <-
    runDiscord
      def
        { discordToken = token
        , discordOnStart = onStart
        , discordOnEnd = onEnd sheeps
        , discordOnEvent = onEvent sheeps
        , discordGatewayIntent =
            def
              { gatewayIntentMembers = True
              , gatewayIntentPrecenses = True
              , gatewayIntentMessageReactions = True
              , gatewayIntentMessageChanges = True
              , gatewayIntentMessageContent = True
              }
        }

  print err
