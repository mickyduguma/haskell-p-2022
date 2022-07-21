
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