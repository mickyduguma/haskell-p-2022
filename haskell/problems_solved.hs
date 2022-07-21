
-- sum every two elements in a list
    addEveryTwo :: [Integer] -> [Integer]
    addEveryTwo [] = []
    addEveryTwo (x:[] ) = [x]
    addEveryTwo (x: (y: ab) ) = (x+y) : addEveryTwo ab