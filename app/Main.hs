module Main where

import System.Process
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import System.Timeout
import Data.Maybe
import Data.List
import Data.Char

data GHCIHandle =
  GHCIHandle
  { pout :: Handle
  , pin :: Handle
  , perr :: Handle
  , processHandle :: ProcessHandle
  }

data GHCIError
  = GHCIError String
  | GHCITimeout
  deriving (Show)

-- | Read handle until it has line ending in given string.
-- | Returning string does not include the ending string.
readHandleUntil :: Handle -> String -> IO String
readHandleUntil handle end = do
  li <- hGetLine handle

  if isInfixOf end li
    then pure ""
    else do
      y <- readHandleUntil handle end
      if null y
        then pure li
        else pure $ li <> "\n" <> y

-- | Start of GHCI process, setup needed things
initializeGHCI :: FilePath -> IO GHCIHandle
initializeGHCI execPath = do
  withCreateProcess
    (proc execPath [])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True}
    $ \(Just pin) (Just pout) (Just perr) processHandle -> do
      hSetBuffering pin LineBuffering
      hSetBuffering pout LineBuffering
      hSetBuffering perr LineBuffering

      hPutStrLn pin ":unset +t +s"
      hPutStrLn pin ":set prompt \"\""
      hPutStrLn pin "import System.IO as COLLIESTD"
      hPutStrLn pin "COLLIESTD.putStrLn \"DONE\""

      readHandleUntil pout "DONE" >>= putStrLn
      pure $ GHCIHandle {..}

closeGHCI :: GHCIHandle -> IO ()
closeGHCI (GHCIHandle {..}) = do
  cleanupProcess (Just pin, Just pout, Just perr, processHandle)

clearGHCIBuffers :: GHCIHandle -> IO ()
clearGHCIBuffers (GHCIHandle {..}) = do
  -- Send interrupt, killing whatever is currently running
  interruptProcessGroupOf processHandle

  -- Make GHCI send cleanup string.
  hPutStrLn pin "COLLIESTD.putStrLn \"COLLIEISCLEANINGUP\""
  hPutStrLn pin "COLLIESTD.hPutStrLn COLLIESTD.stderr \"COLLIEISCLEANINGUPERR\""

  -- Purge until perr/pout have reached the cleanup string
  -- Can I use `void . hGetContents`?
  void $ readHandleUntil pout "COLLIEISCLEANINGUP"
  void $ readHandleUntil perr "COLLIEISCLEANINGUPERR"


sendCommand :: (Handle, Handle, Handle, ProcessHandle) -> String -> IO (Either GHCIError String)
sendCommand (pin, pout, perr, ph) cmd = do
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
        then pure $ Right  x
        else pure $ Left $ GHCIError err
    -- If command hung, and timeout was reached
    Nothing -> do
      -- Stop it
      interruptProcessGroupOf ph
      hPutStrLn pin "COLLIESTD.putStrLn \"CLEANUP\""
      hPutStrLn pin "COLLIESTD.hPutStrLn stderr \"CLEANUPERR\""
      void $ readHandleUntil perr "COLLIESTD.CLEANUPERR"
      void $ readHandleUntil pout "COLLIESTD.CLEANUP"
      pure $ Left $ GHCITimeout

main :: IO ()
main = do
  withCreateProcess
    (proc "ghci" [])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True}
    $ \(Just pin) (Just pout) (Just perr) ph -> do
      hSetBuffering pin LineBuffering
      hSetBuffering pout LineBuffering
      hSetBuffering perr LineBuffering

      hPutStrLn pin ":unset +t +s"
      hPutStrLn pin ":set prompt \"\""
      hPutStrLn pin "import System.IO as COLLIESTD"
      hPutStrLn pin "COLLIESTD.putStrLn \"DONE\""

      readHandleUntil pout "DONE" >>= putStrLn

      putStrLn "done"

      sendCommand (pin, pout, perr, ph) ":{\nfibo = 1 : 1 : zipWith (+) fibo (tail fibo)\n:}" >>= print

      sendCommand (pin, pout, perr, ph) "take 10 fibo" >>= print

      sendCommand (pin, pout, perr, ph) "do {print 1; print 2; pure 3}" >>= print

      putStrLn "done"

      forever $ do
        x <- getLine
        sendCommand (pin, pout, perr, ph) x >>= print
