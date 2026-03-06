cup flOz = \message -> message flOz
getOZ aCup = aCup (\x -> x)
drink aCup ozDrank = if ozDiff > 0 then cup ozDiff else cup 0
   where flOz = getOZ aCup
         ozDiff = flOz - ozDrank
isEmpty aCup = getOZ aCup == 0

