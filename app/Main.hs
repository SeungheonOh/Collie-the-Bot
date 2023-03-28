module Main where

import Sheep.GHCI

import Control.Concurrent
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import System.Environment (lookupEnv)

import Data.Void
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

import Discord
import Discord.Requests qualified as R
import Discord.Types

newtype BotState = BotState
  { ghciHandle :: GHCIHandle
  }

type Parser = P.Parsec Void T.Text

pMention :: Parser Int
pMention = P.between (P.string "<@") (P.char '>') L.decimal

pCodeBlock :: Parser String
pCodeBlock =
  P.try (P.between (P.string "```hs") (P.string "```") (P.many $ P.satisfy (/= '`')))
    P.<|> P.try (P.between (P.string "```") (P.string "```") (P.many $ P.satisfy (/= '`')))
    P.<|> P.between (P.string "`") (P.string "`") (P.many $ P.satisfy (/= '`'))

pCodeMessage :: Parser String
pCodeMessage = do
  void pMention
  void $ P.optional P.newline
  P.space
  pCodeBlock

onStart :: BotState -> DiscordHandler ()
onStart _ = pure ()

onEnd :: IO ()
onEnd = do
  pure ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

mentions :: User -> Message -> Bool
mentions user = elem (userId user) . fmap userId . messageMentions

onEvent :: BotState -> Event -> DiscordHandler ()
onEvent _ (Ready {}) = liftIO $ do
  putStrLn "I'm alive"
onEvent BotState {..} (MessageCreate m) = do
  collie <- cacheCurrentUser <$> readCache
  when (not (fromBot m) && mentions collie m) $ do
    liftIO $ print $ messageContent m
    case P.parse pCodeMessage "" (messageContent m) of
      Left err -> do
        liftIO $ putStrLn $ P.errorBundlePretty err
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "question")
      Right s -> do
        resp <- liftIO $ runCommand ghciHandle s
        let pretty = case resp of
              GHCISuccess "" -> "Okay"
              GHCISuccess x -> "```\n" <> x <> "```"
              GHCIError err -> "```\n" <> err <> "```"
              GHCITimeout -> "Timeout"
        void $ restCall (R.CreateMessage (messageChannelId m) (T.pack pretty))

main :: IO ()
main = do
  token <-
    T.pack . fromMaybe (error "Provide Discord API token via DISCORD_TOKEN envar")
      <$> lookupEnv "DISCORD_TOKEN"

  ghciHandle <- initializeGHCI "ghci" 1000000
  let bstate = BotState {..}

  err <-
    runDiscord
      def
        { discordToken = token
        , discordOnStart = onStart bstate
        , discordOnEnd = onEnd
        , discordOnEvent = onEvent bstate
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
