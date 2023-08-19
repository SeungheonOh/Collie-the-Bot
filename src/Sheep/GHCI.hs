{-# LANGUAGE TypeFamilies #-}

module Sheep.GHCI where

import Sheep.Common
import Sheep.Internal

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

herdFluffyGHCI :: GHCIHandle -> Event -> Cache -> Maybe (DiscordHandler ())
herdFluffyGHCI handle (MessageCreate m) cache = do
  let
    collie = cacheCurrentUser cache
    code = P.parse pCodeMessage "" (messageContent m)

    callsCollie = not (fromBot m) && mentions collie m

  case (callsCollie, code) of
    (True, Right s) -> pure $ do
      lift $ print $ messageContent m
      resp <- lift $ runCommand handle s
      let
        pretty =
          case resp of
            GHCISuccess "" -> "Okay"
            GHCISuccess x -> "```\n" <> x <> "```"
            GHCIError err -> "```\n" <> err <> "```"
            GHCITimeout -> "Timeout"

      void $ restCall (R.CreateMessageDetailed (messageChannelId m) (reply m (T.pack pretty)))
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
          resp <- lift $ runCommand handle s
          let
            pretty =
              case resp of
                GHCISuccess "" -> "Okay"
                GHCISuccess x -> "```\n" <> x <> "```"
                GHCIError err -> "```\n" <> err <> "```"
                GHCITimeout -> "Timeout"

          void $ restCall (R.EditMessage (cid, messageId r) (reply m (T.pack pretty)))
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
