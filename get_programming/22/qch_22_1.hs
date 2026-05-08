main :: IO ()
main = do
  vals <- mapM (\_ -> getLine) [1 .. 3]
  mapM_ putStrLn vals
