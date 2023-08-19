{-# LANGUAGE TypeFamilies #-}

module Sheep.GHCI where

import Sheep.Common
import Sheep.Internal

import Control.Applicative
import Control.Concurrent (
  forkIO,
  killThread,
  newEmptyMVar,
  putMVar,
  readMVar,
 )
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Data.List (find, isInfixOf)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.IO (
  BufferMode (LineBuffering),
  Handle,
  hGetLine,
  hPutStrLn,
  hSetBuffering,
 )
import System.Process (
  CreateProcess (create_group, std_err, std_in, std_out),
  ProcessHandle,
  StdStream (CreatePipe),
  cleanupProcess,
  createProcess,
  interruptProcessGroupOf,
  proc,
 )
import System.Timeout (timeout)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

import Discord
import Discord.Requests qualified as R
import Discord.Types

----------------------------------------------------------------------
-- GHCI handle

data GHCIHandle = GHCIHandle
  { pout :: Handle
  , pin :: Handle
  , perr :: Handle
  , ptimeout :: Int
  , processHandle :: ProcessHandle
  }

data GHCIResponse
  = GHCIError String
  | GHCITimeout
  | GHCISuccess String
  deriving (Show, Eq)

{- | Read handle until it has line ending in given string.
 | Returning string does not include the ending string.
-}
readHandleUntil :: Handle -> String -> IO String
readHandleUntil handle end = do
  li <- hGetLine handle
  if end `isInfixOf` li
    then pure ""
    else do
      y <- readHandleUntil handle end
      if null y
        then pure li
        else pure $ li <> "\n" <> y

-- | Start of GHCI process, setup needed things
initializeGHCI :: FilePath -> Int -> IO GHCIHandle
initializeGHCI execPath ptimeout = do
  (Just pin, Just pout, Just perr, processHandle) <-
    createProcess
      (proc execPath [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        }
  hSetBuffering pin LineBuffering
  hSetBuffering pout LineBuffering
  hSetBuffering perr LineBuffering

  hPutStrLn pin ":unset +t +s"
  hPutStrLn pin ":set prompt \"\""
  hPutStrLn pin ":set prompt-cont \"\""
  hPutStrLn pin ":set prompt-cont-function \"\""
  hPutStrLn pin ":set prompt-function \"\""
  hPutStrLn pin "import System.IO as COLLIESTD"
  hPutStrLn pin "COLLIESTD.putStrLn \"DONE\""
  hPutStrLn pin "COLLIESTD.hPutStrLn COLLIESTD.stderr \"DONE\""

  void $ readHandleUntil pout "DONE"
  void $ readHandleUntil perr "DONE"

  pure $ GHCIHandle {..}

closeGHCI :: GHCIHandle -> IO ()
closeGHCI (GHCIHandle {..}) = do
  cleanupProcess (Just pin, Just pout, Just perr, processHandle)

-- | Unsafe, will run even if GHCI is currently running.
clearGHCI :: GHCIHandle -> IO ()
clearGHCI (GHCIHandle {..}) = do
  -- Send interrupt, killing whatever is currently running
  interruptProcessGroupOf processHandle

  -- Make GHCI send cleanup string.
  hPutStrLn pin "COLLIESTD.putStrLn \"COLLIEISCLEANINGUP\""
  hPutStrLn pin "COLLIESTD.hPutStrLn COLLIESTD.stderr \"COLLIEISCLEANINGUPERR\""

  -- Purge until perr/pout have reached the cleanup string
  -- Can I use `void . hGetContents`?
  void $ readHandleUntil pout "COLLIEISCLEANINGUP"
  void $ readHandleUntil perr "COLLIEISCLEANINGUPERR"

runCommand :: GHCIHandle -> String -> IO GHCIResponse
runCommand handle@GHCIHandle {..} cmd = do
  -- Send command
  hPutStrLn pin cmd

  -- Send strings to stdout/stderr. Used to find where is end of the output
  hPutStrLn pin "COLLIESTD.putStrLn \"COLLIEHASSPOKEN\""
  hPutStrLn pin "COLLIESTD.hPutStrLn stderr \"COLLIEHASSPOKENERR\""

  -- Read stdout with timeout
  outVar <- newEmptyMVar
  outTID <- forkIO $ readHandleUntil pout "COLLIEHASSPOKEN" >>= putMVar outVar
  out <- timeout ptimeout $ readMVar outVar
  killThread outTID

  case out of
    -- Command returned within timeout
    Just x -> do
      err <- readHandleUntil perr "COLLIEHASSPOKENERR"

      if null err
        then pure $ GHCISuccess x
        else pure $ GHCIError err
    -- If command hung, and timeout was reached
    Nothing -> do
      clearGHCI handle
      pure GHCITimeout

----------------------------------------------------------------------
-- Sheep

type Parser = P.Parsec Void T.Text

data Code
  = CodeLine String
  | CodeBlock String
  deriving (Eq, Show)

pMention :: Parser Int
pMention = P.between (P.string "<@") (P.char '>') L.decimal

pCodeBlock :: Parser Code
pCodeBlock =
  P.try (CodeBlock <$> P.between (P.string "```hs") (P.string "```") (P.many $ P.satisfy (/= '`')))
    P.<|> (CodeBlock <$> P.try (P.between (P.string "```haskell") (P.string "```") (P.many $ P.satisfy (/= '`'))))
    P.<|> (CodeBlock <$> P.try (P.between (P.string "```") (P.string "```") (P.many $ P.satisfy (/= '`'))))
    P.<|> (CodeLine <$> P.between (P.string "`") (P.string "`") (P.many $ P.satisfy (/= '`')))

sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = scan
  where
    scan = liftA2 (:) p ((s *> scan) <|> pure [])

pCodeMessage :: Parser [Code]
pCodeMessage = do
  void pMention
  void $ P.many anyChar
  blocks
  where
    anyChar = P.noneOf ("`" :: String)
    blocks = liftA2 (:) pCodeBlock ((P.many anyChar *> blocks) <|> pure [])

runCodeBlock :: GHCIHandle -> Code -> IO GHCIResponse
runCodeBlock handle (CodeLine l) = runCommand handle l
runCodeBlock handle (CodeBlock b) = runCommand handle $ ":{\n" <> b <> "\n:}"

prettyGHCIResponse :: GHCIResponse -> Text
prettyGHCIResponse resp =
  T.pack $
    case resp of
      GHCISuccess "" -> "Okay"
      GHCISuccess x -> "```\n" <> x <> "```"
      GHCIError err -> "```\n" <> err <> "```"
      GHCITimeout -> "Timeout"

herdFluffyGHCI :: GHCIHandle -> Event -> Cache -> Maybe (DiscordHandler ())
herdFluffyGHCI handle (MessageCreate m) cache = do
  let
    collie = cacheCurrentUser cache
    code = P.parse pCodeMessage "" (messageContent m)

    callsCollie = not (fromBot m) && mentions collie m

  case (callsCollie, code) of
    (True, Right s) -> pure $ do
      resps <- lift $ traverse (fmap prettyGHCIResponse . runCodeBlock handle) s

      void $ restCall (R.CreateMessageDetailed (messageChannelId m) (reply m (T.intercalate "\n" resps)))
    _ -> Nothing
herdFluffyGHCI handle (MessageUpdate cid mid) cache = return $ do
  Right m <- restCall (R.GetChannelMessage (cid, mid))

  let
    collie = cacheCurrentUser cache
    code = P.parse pCodeMessage "" (messageContent m)

    callsCollie = not (fromBot m) && mentions collie m

  case (callsCollie, code) of
    (True, Right s) -> do
      Right replies <- restCall (R.GetChannelMessages cid (10, R.AfterMessage mid))

      let targetReply = find (maybe False ((== mid) . messageId) . messageReferencedMessage) replies

      case targetReply of
        Just r -> do
          resps <- lift $ traverse (fmap prettyGHCIResponse . runCodeBlock handle) s
          void $ restCall (R.EditMessage (cid, messageId r) (reply m (T.intercalate "\n" resps)))
        Nothing -> pure ()
    _ -> pure ()
herdFluffyGHCI _ (MessageDelete cid mid) cache = return $ do
  Right replies <- restCall (R.GetChannelMessages cid (20, R.AfterMessage mid))
  let
    collie = cacheCurrentUser cache

    p x =
      Just mid == (messageReference x >>= referenceMessageId)
        && userId (messageAuthor x) == userId collie

    targetReply = find p replies

  case targetReply of
    Just r -> void $ restCall (R.DeleteMessage (cid, messageId r))
    Nothing -> pure ()
herdFluffyGHCI _ _ _ = Nothing

slaughterFluffyGHCI :: GHCIHandle -> IO ()
slaughterFluffyGHCI = undefined

mkFluffyGHCI :: IO Sheep
mkFluffyGHCI = do
  ghciPath <-
    fromMaybe (error "Provide GHCI via COLLIE_GHCI_PATH envvar")
      <$> lookupEnv "COLLIE_GHCI_PATH"

  handle <- initializeGHCI ghciPath 1000000

  return $ Sheep (herdFluffyGHCI handle) (slaughterFluffyGHCI handle)
