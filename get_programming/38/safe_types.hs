safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safeSucc n =
  if n == maxBound
    then Nothing
    else Just (toEnum (fromEnum n + 1))

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x : xs) = xs

safeLast :: [a] -> Either String a
safeLast [] = Left "No last for empty list!"
safeLast ls = safeLast' 10000 ls

safeLast' :: Int -> [a] -> Either String a
safeLast' 0 _ = Left "Max bound of list length is exceeded"
safeLast' _ [x] = Right x
safeLast' n (x : xs) = safeLast' (n - 1) xs
