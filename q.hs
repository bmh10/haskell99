-- Q1
myLast1 = last
myLast2 [x] = x
myLast2 (x:xs) = myLast2 xs
myLast3 = head . reverse

-- Q2
myButLast = head . drop 1 . reverse
myButLast2 = last . init
myButLast3 x = reverse x !! 1
