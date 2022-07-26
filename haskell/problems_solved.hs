
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

    -- problem: i. insert a number in a sorted list in a correct position
    insert' :: Int -> [Int] -> [Int]
    insert' x [] = [x]
    insert' x (ix:ixx) = 
        if x<= ix then 
            x:ix:ixx
        else
            ix: insert' x ixx
--  problem: implement insertion sort 
    isort :: [Int] -> [Int]
    isort [] = []
    isort (i:is) = insert' i (isort is)

    -- problem: Implement Merge 
    merge' :: [Int] -> [Int] -> [Int]
    merge' mx [] = mx
    merge' [] ms = ms
    merge' (mx:mxx) (ms:mss)=
        if mx <= ms then 
            mx: merge' mxx (ms:mss)
        else ms: merge' (mx:mxx) mss
    
