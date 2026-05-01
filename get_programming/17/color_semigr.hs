data Color = Red |
  Yellow |
  Blue |
  Green | 
  Purple |
  Orange |
  White |
  Brown deriving (Show, Eq)
instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b |  a == b = a
    | a == White = b
    | b == White = a
    |  all (`elem` [Red, Blue, Purple]) [a,b] = Purple
    |  all (`elem` [Yellow, Blue, Green]) [a,b] = Green
    |  all (`elem` [Yellow, Red, Orange]) [a,b] = Orange
    |  otherwise = Brown

instance Monoid Color where
  mempty = White
  mappend = (<>)
