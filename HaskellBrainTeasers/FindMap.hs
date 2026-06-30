{-# LANGUAGE ViewPatterns #-}

module HaskellBrainTeasers.FindMap where

import Data.List

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap p (uncons -> Just (p -> Just b, _rest)) = Just b
findMap p (uncons -> Just (_, rest)) = findMap p rest
findMap _ (uncons -> Nothing) = Nothing

main :: IO ()
main = print $ findMap toEven [1, 3, 5, 6, 7, 9]
  where
    toEven :: Int -> Maybe Int
    toEven n = if even n then Just n else Nothing
