data Titan = Cart | Jaw | Female | Armor | Beast| Warhammer | Attack | Colossal | Founding  deriving Enum 

instance Eq Titan where 
 (==) t1 t2 = fromEnum t1 == fromEnum t2 
instance Ord Titan where 
  compare t1 t2 = compare (fromEnum t1) (fromEnum t2)
