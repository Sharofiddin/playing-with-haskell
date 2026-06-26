module HaskellBrainTeasers.SayCheese.NestedIO where

import Text.Read (Lexeme (String))

nestedIO :: IO (IO String)
nestedIO = do
  let nested :: IO String
      nested = do
        putStrLn "I'm Nested!"
        pure "nested"
  putStrLn "Calculating nested IO..."
  pure nested

doesNotRunNestedIO :: IO ()
doesNotRunNestedIO = do
  nested <- nestedIO
  putStrLn "Done!"

runNestedIO :: IO ()
runNestedIO = do
  nested <- nestedIO
  nested
  putStrLn "Done!"
