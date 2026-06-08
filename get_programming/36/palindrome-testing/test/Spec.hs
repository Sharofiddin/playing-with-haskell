import Lib (isPalindrome)

assert :: Bool -> String -> String -> IO ()
assert test passStmt failStmt =
  if test
    then putStrLn passStmt
    else putStrLn failStmt

main :: IO ()
main = do
  putStrLn "Running tests ..."
  assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
  assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
  assert (isPalindrome "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
  assert (isPalindrome ":racecar:") "passed ':racecar:'" "FAIL: ':racecar:'"
  assert ((not . isPalindrome) "cat") "passed 'cat'" "FAIL: 'cat'"
  putStrLn "done"
