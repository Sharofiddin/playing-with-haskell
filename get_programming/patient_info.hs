data Gender = Male | Female
type PatientName = (String, String)
type PatientHeight = Int
type PatientAge = Int
patientInfo :: PatientName->PatientAge->PatientHeight->String
patientInfo patientName age height = 
  name ++ " " ++ ageHeight 
  where name = snd patientName ++ " " ++ fst patientName
        ageHeight = "(" ++ show age ++ " yrs. " ++ show height ++ "in.)"
