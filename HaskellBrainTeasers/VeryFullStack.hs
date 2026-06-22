module HaskellBrainTeasers.VeryFullStack where

data RoseTree a = RoseTree a [RoseTree a]

breadthFirst :: RoseTree a -> [a]
breadthFirst (RoseTree a trees) = a : go trees
  where
    go [] = []
    go subtrees =
      let (vals, nextSubtrees) =
            foldr accumRoseTree ([], []) subtrees
       in vals <> go nextSubtrees
    accumRoseTree ~(RoseTree val subtrees) (vals, allSubTrees) = (val : vals, subtrees <> allSubTrees)

main :: IO ()
main =
  if "Maple" `elem` breadthFirst forest
    then putStrLn "Pancake time!"
    else putStrLn "No maple syrup"
  where
    forest =
      RoseTree
        "Aspen"
        [ RoseTree
            "Baobab"
            [RoseTree "Paw Paw" [], RoseTree "Basswood" []],
          RoseTree
            "Cherry"
            [RoseTree "Maple" [], error "boom"]
        ]

infiniteTree :: RoseTree Int
infiniteTree = go 0
  where
    go n = RoseTree n [go (n + 1)]
