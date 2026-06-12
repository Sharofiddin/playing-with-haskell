module Main (main) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Array.Base
import Data.Array.ST (runSTUArray)

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. (end - i)] $ \j -> do
      val <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray (j + 1) val
        writeArray stArray j nextVal
  return stArray

crossover :: UArray Int Int -> UArray Int Int -> Int -> UArray Int Int
crossover arr1 arr2 cutoff = runSTUArray $ do
  stArray1 <- thaw arr1
  let end = (snd . bounds) arr1
  forM_ [cutoff .. end] $ \i -> do
    writeArray stArray1 i (arr2 ! i)
  return stArray1

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [0 .. end] $ \i -> do
    val <- readArray stArray i
    when (val == 0) $ do
      writeArray stArray i (-1)
  return stArray

main :: IO ()
main = print (bubbleSort myData)
