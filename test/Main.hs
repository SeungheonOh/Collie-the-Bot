module Main where

import Sheep.GHCI (pCodeBlock, Code(..))

import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Text.Megaparsec (parse)
import Data.Text qualified as T

getRight :: Either e a -> Maybe a
getRight (Left _) = Nothing
getRight (Right a) = Just a

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

  test (CodeLine "True")      "`True`False`"
  test (CodeLine "False")   "\\`True`False`"
  test (CodeLine "True")  "\\\\`True`False`"
  test (CodeLine "\\")          "`\\`False`"

  test (CodeLine "7 `mod` 4")       "``7 `mod` 4``"
  test (CodeLine "7 ")            "\\``7 `mod` 4``"
  test (CodeLine "7 `mod` 4")   "\\\\``7 `mod` 4``"
  test (CodeLine "`7 ")              "``7 `mod` 4`"

  assertEqual "a`b``" Nothing (getRight $ parse pCodeBlock "" "a`b``")
  assertEqual  "`b``" Nothing (getRight $ parse pCodeBlock ""  "`b``")

  test (CodeLine "a``b") "`a``b`"

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
