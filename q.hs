-- Q1
myLast1 = last
myLast2 [x] = x
myLast2 (x:xs) = myLast2 xs
myLast3 = head . reverse

-- Q2
myButLast = head . drop 1 . reverse
myButLast2 = last . init
myButLast3 x = reverse x !! 1

-- Q3
elementAt l k = l !! (k-1)
elementAt22 l k = last $ take k l

-- Q4
myLength = length
myLength2 = foldl (\acc x -> acc + 1) 0
myLength3 xs = sum $ map (+1) $ map (*0) xs
myLength4 = sum . map(\_ -> 1)




