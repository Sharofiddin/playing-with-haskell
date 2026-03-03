import Data.Char (toLower)
isPalindorme xs =  cleaned == reverse cleaned where
    lower = map toLower xs
    cleaned = filter ( /=' ') lower
