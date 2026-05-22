import Data.Map qualified as Map

data Grade = F | E | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int,
    codeReview :: Grade,
    cultureFit :: Grade,
    education :: Degree
  }

viable :: Candidate -> Bool
viable candidate = and tests
  where
    codeReviewTest = codeReview candidate > B
    cultureFitTest = cultureFit candidate > C
    educationTest = education candidate >= MS
    tests = [codeReviewTest, cultureFitTest, educationTest]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

readInt :: IO Int
readInt = getLine >>= return . read

readDegree :: IO Degree
readDegree = getLine >>= return . read

readGrade :: IO Grade
readGrade = getLine >>= return . read

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "Enter ID:"
  cId <- readInt
  putStrLn "Enter code grade:"
  cCodeGrade <- readGrade
  putStrLn "Enter culture fit grade:"
  cCultureGrade <- readGrade
  putStrLn "Enter education degree:"
  cDegree <- readDegree
  return
    Candidate
      { candidateId = cId,
        codeReview = cCodeGrade,
        cultureFit = cCultureGrade,
        education = cDegree
      }

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  return
    ( if viable candidate
        then "passed"
        else "failed"
    )

c1 :: Candidate
c1 =
  Candidate
    { candidateId = 1,
      codeReview = A,
      cultureFit = A,
      education = MS
    }

candidate1 :: Candidate
candidate1 =
  Candidate
    { candidateId = 1,
      codeReview = A,
      cultureFit = A,
      education = BA
    }

candidate2 :: Candidate
candidate2 =
  Candidate
    { candidateId = 2,
      codeReview = C,
      cultureFit = A,
      education = PhD
    }

candidate3 :: Candidate
candidate3 =
  Candidate
    { candidateId = 3,
      codeReview = A,
      cultureFit = B,
      education = MS
    }

candidateDB :: Map.Map Int Candidate
candidateDB =
  Map.fromList
    [ (1, candidate1),
      (2, candidate2),
      (3, candidate3)
    ]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe id = do
  candidate <- Map.lookup id candidateDB
  return
    ( if viable candidate
        then "passed"
        else "failed"
    )

assessCandidate :: (Monad m) => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  return
    ( if viable candidate
        then "passed"
        else "failed"
    )
