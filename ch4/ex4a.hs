(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False
