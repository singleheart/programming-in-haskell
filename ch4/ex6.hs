(&&) :: Bool -> Bool -> Bool
a && b =
  if a == True
    then b
    else False
