type FirstName = String
type LastName = String
type MiddleName = String

data Sex = Male | Female
data Name = Name FirstName LastName |
            NameWithMiddle FirstName MiddleName LastName
data RhType = Pos | Neg
data ABOType = A | B | AB | O 
data BloodType = BloodType ABOType RhType  
--canDonateTo :: BloodType -> BloodType -> Bool
--canDonateTo (BloodType O _) _ = True
--canDonateTo _ (BloodType AB _) = True
--canDonateTo (BloodType A _) (BloodType A _) = True
--canDonateTo (BloodType B _) (BloodType B _) = True
--canDonateTo _ _ = False
data Patient = Patient { name ::  Name,
                         sex :: Sex,
                         age :: Int,
                         weight :: Int,
                         height :: Int,
                         bloodType :: BloodType }
showABOType :: ABOType -> String
showABOType A = "A"
showABOType B = "B"
showABOType O = "O"
showABOType AB = "AB"

showRhType :: RhType -> String
showRhType Pos = "+"
showRhType Neg = "-"
showName :: Name -> String
showName (Name f l) = f ++ " " ++  l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABOType abo ++  showRhType rh



canDonateTo :: Patient -> Patient -> Bool

canDonateTo (Patient {bloodType = (BloodType O _)}) _ = True 
canDonateTo _ (Patient {bloodType = (BloodType AB _)}) = True 
canDonateTo (Patient {bloodType = (BloodType A _)}) (Patient {bloodType = (BloodType A _)}) = True 
canDonateTo (Patient {bloodType = (BloodType B _)}) (Patient {bloodType = (BloodType B _)})  = True 
canDonateTo _ _ = False
showGender :: Sex -> String
showGender Male = "Male"
showGender Female = "Female"

patientSummary :: Patient -> String
patientSummary patient = 
   "**********" ++
   "\nPatient Name: " ++ showName (name patient) ++ 
   "\nSex: " ++ showGender (sex patient) ++  
   "\nAge: " ++ show (age patient) ++
   "\nHeight: " ++ show (height patient) ++ " in." ++  
   "\nWeight: " ++ show (weight patient) ++ " lbs." ++
   "\nBlood Type: " ++ showBloodType (bloodType patient) ++
   "\n**********"
