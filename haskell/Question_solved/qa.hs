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

-- Question: takes an element and a count and returns the list which is that element repeated that many times
    replicat 0 _     = []
    replicat n given = given : replicat (n-1) given

-- Multiple list using highorder functions
    multList n [] = []
    multList n (x:xs) = n*x : multList n xs
    
    tripleList = multList 3
    doubleList = multList 2

