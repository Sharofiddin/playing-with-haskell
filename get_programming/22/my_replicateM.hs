myReplicateM :: (Monad m) => m a -> Int -> m [a]
myReplicateM action n = mapM (\_ -> action) [1 .. n]
