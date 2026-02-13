nyOffice name = nameText ++ ": PO Box 789 -New York, NY, 10013"
  where lastName = snd name 
        nameText = fst name ++ " " ++ lastName

sfOffice name = if lastName < "L"
                   then nameText
                        ++ " - PO Box 1234 - San Francisco, CA, 94111"
                   else nameText
                        ++ " - PO Box 1010 - San Francisco, CA, 94109"
        where lastName = snd name
              nameText = fst name ++ " " ++ lastName
renoOffice name = nameText ++ " PO Box 456 - Reno, NV, 89523"
  where nameText = snd name
washingtonOffice name = nameText ++ "Esq PO Box 999 - Washington, DC, 98004"
  where nameText = fst name ++ " " ++ snd name
getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "wa" -> washingtonOffice
  _ -> (\name -> fst name ++ " " ++ snd name)

addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location
