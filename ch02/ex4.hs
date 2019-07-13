last' xs = head (reverse xs)
last'' xs = head (drop (length xs - 1) xs)