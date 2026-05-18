-- 27.1
data Box a = Box a deriving (Show)

instance Functor Box where
  fmap func (Box a) = Box (func a)

mirror :: Int -> a -> [a]
mirror 0 _ = []
mirror n x = x : mirror (n - 1) x

morePresents :: Int -> Box a -> Box [a]
morePresents n = fmap (mirror n)

-- 27.2
myBox :: Box Int
myBox = Box 1

unwrap :: Box a -> a
unwrap (Box x) = x
