import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity bits = sum bits `mod` 2

addParity :: [Bit] -> [Bit]
addParity bits = parity bits : bits

encode :: String -> [Bit]
encode = concatMap (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity bits
  | head bits == parity (tail bits) = tail bits
  | otherwise = error "Parity Error"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode
