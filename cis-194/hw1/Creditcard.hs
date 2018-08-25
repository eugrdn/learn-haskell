mod10 :: Integer -> Integer
mod10 x = mod x 10

div10 :: Integer -> Integer
div10 x = div x 10

-- ex1

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (div10 n) ++ [mod10 n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod10 n : toDigitsRev (div10 n)

-- ex2

toSingle :: Integer -> Integer
toSingle x
  | x == 0 = 0
  | otherwise = toSingle (div10 x) + mod10 x

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [toSingle x | x <- xs]

-- ex3

double :: Integer -> Integer
double x = x * 2

doubleFirstOfPair :: [Integer] -> [Integer]
doubleFirstOfPair (x : y : xs) = double x : y : doubleFirstOfPair xs
doubleFirstOfPair (x : y) = double x : y
doubleFirstOfPair x = x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | null xs = []
  | odd (length xs) = head xs : doubleFirstOfPair (tail xs)
  | otherwise = doubleFirstOfPair xs

-- ex4

validate :: Integer -> Bool
validate n = mod10 (sumDigits (doubleEveryOther (toDigits n))) == 0

------------------------------------------------------------

result :: Bool
result = validate 4012888888881881

main = print result
