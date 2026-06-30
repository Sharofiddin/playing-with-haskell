{-# LANGUAGE OrPatterns #-}
module HaskellBrainTeasers.Bismuth where 

data Color = Black|White|Gray|Red|Green|Blue|Yellow|Purple|Orange deriving Show

isAchromatic :: Color -> Bool
isAchromatic (Black;White;Gray) = True
isAchromatic(Red;Green;Blue;Yellow;Purple;Orange) = False

main :: IO()
main = print (isAchromatic Red) >> print (isAchromatic Gray)
