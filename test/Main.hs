module Main where

import Sheep.GHCI (pCodeBlock, Code(..))

import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Text.Megaparsec (parse)
import Data.Text qualified as T

assertEqual :: (MonadFail m, Eq a, Show a, HasCallStack) => String -> a -> a -> m ()
assertEqual message expected actual
  | expected == actual = return ()
  | otherwise = fail . unlines $
      [ ""
      , ""
      , "### Failed: " <> message
      , "  expected: " <> show expected
      , "    actual: " <> show actual
      , ""
      , prettyCallStack callStack
      , ""
      ]

testParseCode :: MonadFail m => m ()
testParseCode = do
  let test expected text = assertEqual (show text) (Right expected) (parse pCodeBlock "" text)

  test (CodeLine "hello") "`hello`"

  uncurry test
    ( CodeBlock . unlines $
      [ ""
      , "let a = a in a"
      ]
    , T.unlines
      [ "```haskell"
      , "let a = a in a"
      , "```"
      ]
    )

  uncurry test
    ( CodeBlock . unlines $
      [ ""
      , "7 `mod` 4"
      ]
    , T.unlines
      [ "```haskell"
      , "7 `mod` 4"
      , "```"
      ]
    )

main :: IO ()
main = do
  testParseCode
