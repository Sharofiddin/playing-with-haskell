ifEven f x = if even x 
                then f x
                else x

genIfXEven f = (\x -> ifEven f x)
