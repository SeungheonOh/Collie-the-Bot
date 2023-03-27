module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO
import System.Process
import System.Timeout

data GHCIHandle = GHCIHandle
  { pout :: Handle
  , pin :: Handle
  , perr :: Handle
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
  li <- hGetLine handler
  if end `isInfixOf` li
    then pure ""
    else do
      y <- readHandleUntil handle end
      if null y
        then pure li
        else pure $ li <> "\n" <> y

-- | Start of GHCI process, setup needed things
initializeGHCI :: FilePath -> IO GHCIHandle
initializeGHCI execPath = do
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

sendCommand :: GHCIHandle -> String -> IO GHCIResponse
sendCommand (GHCIHandle {..}) cmd = do
  -- Send command
  hPutStrLn pin cmd

  -- Send strings to stdout/stderr. Used to find where is end of the output
  hPutStrLn pin "COLLIESTD.putStrLn \"COLLIEHASSPOKEN\""
  hPutStrLn pin "COLLIESTD.hPutStrLn stderr \"COLLIEHASSPOKENERR\""

  -- Read stdout with timeout
  outVar <- newEmptyMVar
  outTID <- forkIO $ readHandleUntil pout "COLLIEHASSPOKEN" >>= putMVar outVar
  out <- timeout 1000000 $ readMVar outVar
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

main :: IO ()
main = do
  ghciHandle <- initializeGHCI "ghci"
  sendCommand ghciHandle "fibo = 1 : 1 : zipWith (+) (tail fibo) fibo" >>= print
  sendCommand ghciHandle "fibo" >>= print

  sendCommand ghciHandle "take 10 fibo" >>= print
  sendCommand ghciHandle "take 2 fibo" >>= print
  sendCommand ghciHandle "take 3 fibo" >>= print
  sendCommand ghciHandle "take 5 fibo" >>= print

  closeGHCI ghciHandle
