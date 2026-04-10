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
bitsToInt bits = sum (map (\x-> 2^snd x) trueLocations)
     where size = length bits
           indices = [size-1, size-2 .. 0]
           trueLocations = filter (\x-> fst x == True)
                           (zip bits indices)
bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

xorBool :: Bool -> Bool -> Bool
xorBool True True = False
xorBool False False = False
xorBool _ _ = True

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)

applyOTP' :: String -> String -> [Bits]
applyOTP' pad text = map (\pair -> (fst pair) `xor` snd pair) (zip padBits plainTextBits)
          where padBits = map charToBits pad
                plainTextBits = map charToBits text
applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar (applyOTP' pad plainText)

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where 
   encode Rot  = rotEncoder 
   decode Rot  = rotDecoder 

data OneTimePad = OTP String

instance Cipher OneTimePad where
   encode (OTP pad) = applyOTP pad
   decode (OTP pad) = applyOTP pad

