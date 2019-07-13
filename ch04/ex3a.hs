safetail :: [a] -> [a]
safetail xs =
  if null xs
    then []
    else tail xs
