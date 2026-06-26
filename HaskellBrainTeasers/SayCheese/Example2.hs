module HaskellBrainTeasers.SayCheese.Example1 where

unnecessary :: IO Int
unnecessary = do
  putStrLn "This is Unnecessary"
  pure 5

main :: IO ()
main = do
  let _unused = unnecessary
  putStrLn "HW"
