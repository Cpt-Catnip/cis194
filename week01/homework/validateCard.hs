-- file: week01/homework/validateCard.hs

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = mod x 10 : toDigitsRev (div x 10)

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x : y : zs) = x : 2 * y : doubleEveryOther' zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOther' (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x < 10 = x
  | otherwise = sumDigits (toDigits x)
sumDigits (x : xs) = sumDigits [x] + sumDigits xs

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0
