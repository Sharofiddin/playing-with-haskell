example :: Maybe Int
example = pure (*) <*> pure ((+) 2 4) <*> pure 6
