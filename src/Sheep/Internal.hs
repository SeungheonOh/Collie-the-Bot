{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Sheep.Internal where

class Sheep a where
  type SheepResponse a
  type SheepAction a
  initialize :: IO a
  herd :: SheepAction a -> IO (SheepResponse a)
  slaughter :: a -> IO ()
