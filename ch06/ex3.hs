(^) :: (Integral a) => a -> a -> a
(^) _ 0 = 1
(^) a b
  | b > 0 = a * (a Main.^ (b - 1))

{-
2 ^ 3
= {applying ^}
2 * (2 ^ 2)
= {applying ^}
2 * (2 * (2 ^ 1))
= {applying ^}
2 * (2 * (2 * (2 ^ 0)))
= {applying ^}
2 * (2 * (2 * (1)))
= {applying *}
2 * (2 * (2))
= {applying *}
2 * (4)
= {applying *}
8
-}