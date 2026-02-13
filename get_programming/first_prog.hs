main = do 
  print "Who is the email for?"
  recipient <- getLine 
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)
toPart recipient = "Dear " ++ recipient ++ ", \n"
fromPart sender = "Thanks, \n" ++ sender
bodyPart item = "Thanks for buying " ++ item ++ ".\n"
createEmail recipient item sender = toPart recipient ++
                                    bodyPart item ++
                                    fromPart sender
