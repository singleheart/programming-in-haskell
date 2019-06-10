import Data.Char

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

alphabets :: [Char] -> Int
alphabets xs =
  length [x | x <- xs, (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')]

let2int :: Char -> Int
let2int c
  | isLower c = ord c - ord 'a'
  | otherwise = ord c - ord 'A'

int2letLower :: Int -> Char
int2letLower n = chr (ord 'a' + n)

int2letUpper :: Int -> Char
int2letUpper n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2letLower ((let2int c + n) `mod` 26)
  | isUpper c = int2letUpper ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> [Char] -> [Char]
encode n xs = [shift n x | x <- xs]

table :: [Float]
table =
  [ 8.1
  , 1.5
  , 2.8
  , 4.2
  , 12.7
  , 2.2
  , 2.0
  , 6.1
  , 7.0
  , 0.2
  , 0.8
  , 4.0
  , 2.4
  , 6.7
  , 7.5
  , 1.9
  , 0.1
  , 6.0
  , 6.3
  , 9.0
  , 2.8
  , 1.0
  , 2.4
  , 0.2
  , 2.0
  , 0.1
  ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs =
  [ percent ((count l xs) + (count u xs)) n
  | (l, u) <- zip ['a' .. 'z'] ['A' .. 'Z']
  ]
  where
    n = alphabets xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: [Char] -> [Char]
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs
