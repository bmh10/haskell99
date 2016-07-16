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
myLength5 = fst . last . zip [1..]

-- Q5
myReverse = reverse
myReverse2 [] = []
myReverse2 (x:xs) = myReverse2 xs ++ [x] 
myReverse3 = foldl (flip (:)) []

-- Q6
isPalindrome s = s == reverse s
isPalindrome2 [] = True
isPalindrome2 [_] = True
isPalindrome2 xs = (head xs) == (last xs) && (isPalindrome2 $ init $ tail $ xs) 

-- Q7
data NestedList a = Elem a | List [NestedList a]

flatten (Elem a) = [a]
flatten (List a) = concatMap flatten a

flatten2 (Elem a) = [a]
flatten2 (List []) = []
flatten2 (List (x:xs)) = flatten2 x ++ flatten2 (List xs)


