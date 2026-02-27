general3ToAll f xs = f xs

add3ToAll [] = []
add3ToAll (x:xs) = (x+3) :add3ToAll xs

multiple3ToAll [] = []
multiple3ToAll (x:xs) = (x * 3) :multiple3ToAll xs
