rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation 
  where halfAlphabet = alphabetSize `div` 2
        offset = fromEnum c + halfAlphabet
        rotation = offset `mod` alphabetSize
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Enum, Bounded, Show)
fourLetterAlphabetEncoder :: [FourLetterAlphabet]-> [FourLetterAlphabet]

fourLetterAlphabetEncoder vals = map rot4l vals 
  where alphabetSize = 1 + fromEnum( maxBound :: FourLetterAlphabet)
        rot4l = rotN alphabetSize

rotNDecoder :: (Bounded a, Enum a) => Int -> a ->a
rotNDecoder alphabetSize c = toEnum rotation 
  where halfA = alphabetSize `div` 2
        offset = if even alphabetSize 
        then halfA + fromEnum c 
        else halfA + fromEnum c + 1
        rotation = offset `mod` alphabetSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
   where alphabetSize = 1 + fromEnum ( maxBound :: Char)
         rotChar = rotN alphabetSize
rotDecoder :: String -> String
rotDecoder encryptedText = map rotCharDecoder encryptedText
   where alphabetSize = 1 + fromEnum( maxBound :: Char)
         rotCharDecoder = rotNDecoder alphabetSize

type Bits = [Bool]
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
          where remainder = n `mod` 2
                nextVal = n `div` 2
intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
   where reversedBits = reverse (intToBits' n)
         maxBits = length (intToBits' maxBound)
         missingBits = maxBits - (length reversedBits)
         leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt :: Bits -> Int

