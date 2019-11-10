data Tree
  = Leaf Int
  | Node Tree Tree
  deriving (Show)

countLeaves :: Tree -> Int
countLeaves (Leaf _) = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

countNodes :: Tree -> Int
countNodes (Leaf _) = 0
countNodes (Node l r) = countNodes l + 1 + countNodes r
