data Grade = F | E | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BC | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

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

c1 :: Candidate
c1 =
  Candidate
    { candidateId = 1,
      codeReview = A,
      cultureFit = A,
      education = MS
    }
