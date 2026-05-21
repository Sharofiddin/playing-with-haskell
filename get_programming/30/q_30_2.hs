allApp :: (Monad m) => m (a -> b) -> m a -> m b
allApp mFunc val = mFunc >>= (\f -> val >>= (\x -> return (f x)))

val :: Maybe Int
val = Just 8

maybeFunc :: Maybe (Int -> String)
maybeFunc = Just (\i -> show (i + 2))

maybeRsStr :: Maybe String
maybeRsStr = allApp maybeFunc val
