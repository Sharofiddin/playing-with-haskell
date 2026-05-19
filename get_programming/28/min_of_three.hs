minOfThree :: Int -> Int -> Int -> Int
minOfThree v1 v2 v3 = min v1 (min v2 v3)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt
main :: IO ()
main = do
    putStrLn "Enter numbers"
    min <- minOfInts
    putStrLn ("Min of three is " ++ show min)
