robot (name,attack,hp) = \msg -> msg (name,attack,hp)
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,h) = h
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

printRobot aRobot = aRobot (\(n,a,h) -> n ++ 
                                 " attack: " ++ show a ++
                                 " hp: " ++ show h)

setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

damage aRobot attackDamage = aRobot (\(n,a,h) -> robot(n,a,h-attackDamage))
fight aRobot defender = damage defender attack 
    where attack = if getHp aRobot > 10 
                      then getAttack aRobot 
                      else 0
