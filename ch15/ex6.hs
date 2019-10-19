sqroot :: Double -> Double
sqroot n = snd . head $ dropWhile pred (zip it (tail it))
  where
    approx = 0.00001
    pred (x,y) = abs (x - y) > approx
    it = iterate next n
    next a = (a + n / a) / 2
