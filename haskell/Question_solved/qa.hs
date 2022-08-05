-- Removing uppercase leters
    removeUppercase :: String -> String
    removeUppercase ru = [x | x <- ru, x `elem` ['a'..'z']]

-- filtering lists with even elements from a nested list
    nestedList :: [[Int]] -> [[Int]]
    nestedList nl = [ [a | a <- al, even a] | al <- nl ]

-- which right triangle that has integers for all sides and all sides equal to or smaller than 10
-- And the perimeter should be equal to  24? 
    rightTriangle = [(a,b,c) | c <- [1..10], b <- [1..c], a <-[1..b], a^2 + b^2 == c^2, a+b+c == 24 ]

-- custom length function using pattern matching
    length' :: (Num b) => [a] -> b
    length' [] = 0
    length' (_:xs) = 1 + length' xs

--  BMI calculator and its results 
    bmical :: Double -> Double -> Double
    bmical w h = w/(h^2)

    bmicon:: Double->Double -> String 
    bmicon weight height 
        | bmical weight height  <= 18.5 = "You are under weight because "
        | bmical weight height  <= 25.0 = "You are normal"
        | bmical weight height  <= 30.0 = "You are overweight "
        | otherwise = "you are obese"
    -- bmicon:: Double->Double -> String 
    -- bmicon weight height 
    --     | bmi  <= 18.5 = "You are under weight because "
    --     | bmi <= 25.0 = "You are normal"
    --     | bmi <= 30.0 = "You are overweight "
    --     | otherwise = "you are obese"
    --    where bmi = weight / (height)^2

--  a program to get list of weight and height pairs and gives list of bmi
    listbmi :: [(Double,Double)]-> [Double]
    listbmi lb = [ bmical bw bh | (bw, bh) <- lb]

-- Question: takes an element and a count and returns the list which is that element repeated that many times
    replicat 0 _     = []
    replicat n given = given : replicat (n-1) given

-- Multiple list using highorder functions
    multList n [] = []
    multList n (x:xs) = n*x : multList n xs
    
    tripleList = multList 3
    doubleList = multList 2

--  take using recursion 
    take' :: (Num a) => Int -> [a] -> [a]
    take' num [] = []
    take' num (t:txs)
        | num <= 0 = []
        | num > length txs + 1 = error "Not enought elements"
        | otherwise = t : take' (num-1) txs
--  implementation of zip function using recursion
    zip' :: [a]-> [b] -> [(a,b)]
    zip' _ [] = []
    zip' [] _ = []
    zip' (az:azip) (bz:bzip) = (az,bz): zip' azip bzip

-- count number of strings found in a string
    count :: Char -> String -> Int
    count x xs = length [x' | x' <- xs, x == x']


--  Give frequency of lowercase lettes in a given String in a percent
    lowercase :: String -> Int
    lowercase xs = length [x | x <- xs, x >= 'a' && x <= 'z']   

    percent :: Int -> Int -> Float
    percent n m = (fromIntegral n / fromIntegral m) * 100

    frequencyL :: String -> [Float]
    frequencyL xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowercase xs 


--  Find the index of the given number in the list 
--  If the number is found in the list more than 1, the function should 
--  find all indexes in the list
    positions :: Eq a => a -> [a] -> [Int]
    positions x xs = [i | (x',i) <- zip xs [0..n], x == x']
                    where n = length xs - 1