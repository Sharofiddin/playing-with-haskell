allFmapM :: (Monad m) => (a -> b) -> m a -> m b
allFmapM func x = x >>= (\el -> return (func el))

maybeRes :: Maybe Int
maybeRes = allFmapM (+ 2) (Just 4)
