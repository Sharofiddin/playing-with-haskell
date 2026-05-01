import qualified Data.Map as Map

data Box a = Box a deriving Show
data Triple a = Triple a a a deriving Show

wrap :: a -> Box a
wrap  = Box 
unwrap :: Box a -> a
unwrap (Box x) = x

mapBox :: (a->b) -> Box a -> Box b
mapBox f (Box a) =  Box (f a)

mapTriple :: (a->b) -> Triple a -> Triple b
mapTriple f (Triple x y z) =   Triple (f x) (f y) (f z)


data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)
organs :: [Organ]
organs = [Heart, Brain, Kidney, Spleen]
counts :: [Int]
counts = [2,1,1,2]
organPairs :: [(Organ,Int)]
organPairs = zip organs counts
organCatalog :: Map.Map Organ Int
organCatalog = Map.fromList organPairs
