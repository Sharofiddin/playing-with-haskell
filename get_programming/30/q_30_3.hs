bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just x) func = func x

mybInt :: Maybe Int
mybInt = Just 6

func :: Int -> Maybe String
func a = Just (show (a + 2))

mybString :: Maybe String
mybString = bind mybInt func

noth :: Maybe String
noth = bind Nothing func
