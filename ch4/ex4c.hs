(||) :: Bool -> Bool -> Bool
True || _ = True
False || a = a
