module HaskellBrainTeasers.OhEmDash where

isIdentity :: (Int -> Int) -> String
isIdentity f = show $ or [f n == n | n <- [0 .. 10]]

main :: IO ()
main = do
  putStrLn $ "*1 " <> isIdentity (* 1)
  putStrLn $ "+0 " <> isIdentity (+ 0)
  putStrLn $ "-0 " <> isIdentity (subtract 0)
