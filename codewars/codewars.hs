narcissistic :: Int -> Bool
narcissistic n = let digitsLength = length $ show n
                     digits = toDigitList n
                in foldr (\x acc -> acc + (x^digitsLength) ) 0 digits == n

toDigitList :: Integral x => x -> [x]
toDigitList 0 = []
toDigitList n = toDigitList( n `div` 10 ) ++ [n `mod` 10]