class (Eq a, Enum a) => Dice a where
  roll :: Int -> a
data SixSidedDice = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)
instance Show SixSidedDice where 
  show S1 = "1"
  show S2 = "2"
  show S3 = "3"
  show S4 = "4"
  show S5 = "5"
  show S6 = "6"
data FiveSideDice = One | Two | Three | Four | Five deriving (Eq, Enum, Show)

instance Dice FiveSideDice where 
 roll n = toEnum (n `mod` 5)
