allFmap :: (Applicative f) => (a -> b) -> f a -> f b
allFmap func x = (pure func) <*> x
