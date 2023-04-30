-- file: week01/homework/validateCard.hs

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = mod x 10 : toDigitsRev (div x 10)
