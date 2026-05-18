reverseMaybeStr :: Maybe String -> Maybe String
reverseMaybeStr maybeStr = reverse <$> maybeStr
