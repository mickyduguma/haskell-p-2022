
-- sum every two elements in a list
    addEveryTwo :: [Integer] -> [Integer]
    addEveryTwo [] = []
    addEveryTwo (x:[] ) = [x]
    addEveryTwo (x: (y: ab) ) = (x+y) : addEveryTwo ab

    -- quick_sort 
    quickSort :: [Integer] -> [Integer]
    quickSort [] = []
    quickSort (q:qs) = quickSort ys ++ [q] ++ quickSort yz
        where ys = [ a | a <- qs, a <=  q ]
              yz = [ b | b<- qs , b> q]
    

    -- validating credit card
    toDigits    :: Integer -> [Integer]
    toDigitsRev :: Integer -> [Integer]
    toDigitsRev n
        | n <= 0 = []
        | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

    toDigits = reverse . toDigitsRev

    doubleEveryOther :: [Integer] -> [Integer]    
    doubleEveryOther [] = []
    doubleEveryOther (x:[]) = [x]
    doubleEveryOther (x:y:zs) = x : (2 * y) : doubleEveryOther zs

    doubleEveryOtherRev = reverse . doubleEveryOther . reverse

    sumDigits :: [Integer] -> Integer    
    sumDigits [] = 0
    sumDigits (x:xs)
        | x < 10    = x + sumDigits xs
        | otherwise = (x `mod` 10) + (x `div` 10) + sumDigits xs

    validate :: Integer -> Bool
    validate n = (mod (sumDigits (doubleEveryOtherRev (toDigits n))) 10) == 0