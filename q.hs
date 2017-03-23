import Data.List
import System.Random

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

-- Q8
compress [] = []
compress [x] = [x]
compress (x:y:xs)
 | x == y = [x] ++ compress xs
 | otherwise = [x] ++ compress (y:xs) 

compress2 :: Eq a => [a] -> [a]
compress2 = map head . group 

compress3 [] = []
compress3 (x:xs) = x : (compress3 $ dropWhile (==x) xs)

-- Q9
pack :: Eq a => [a] -> [[a]]
pack = group

pack2 :: Eq a => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x : takeWhile (== x) xs) : pack2(dropWhile (== x) xs)

pack3 :: Eq a => [a] -> [[a]]
pack3 [] = []
pack3 (x:xs) = let (first, rest) = span (== x) xs
               in (x:first) : pack3 rest

-- Q10
encode xs = zip a b 
            where grp = group xs
                  a = map length grp
                  b = map head grp

encode2 xs = map (\x -> (length x, head x)) (group xs)

encode3 xs = [(length x, head x) | x <- group xs]

-- Q11
data EncodeElem a = Multiple Int a | Single a
  deriving (Show)

encodeModified :: Eq a => [a] -> [EncodeElem a]
encodeModified = map encodeHelper . encode
  where encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x

-- Q12
decodeModified :: [EncodeElem a] -> [a]
decodeModified = concatMap decodeHelper
  where decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x

-- Q13


-- Q14
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

dupli2 [] = []
dupli2 (x:xs) = x:x:dupli2 xs

-- Q15
repli xs n = concatMap (replicate n) xs
repli2 = flip $ concatMap . replicate

-- Q16
dropEvery xs n = map snd [(i, x) | (i, x) <- zip [1..] xs, i `mod` n /= 0]

dropEvery2 xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])

-- Q17
split xs n = (take n xs, drop n xs)
split2 = flip splitAt

-- Q18
slice xs s e = drop (s-1) $ take e xs

-- Q19
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n
 | n > 0 = rotate (xs++[x]) (n-1)
 | n < 0 = rotate (last xs : x : init xs) (n+1)

-- Q20
removeAt n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)

-- Q21
insertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

insertAt2 x xs n = ys ++ x:zs where (ys, zs) = Main.split xs (n-1)

-- Q22
range lb ub = [lb..ub]

--range2 = enumFromTo

range3 x y = take (y-x+1) $ iterate (+1) x

-- Q23
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]


-- Q31 Is number prime
isPrime 1 = False
isPrime n = not $ any (\x -> mod n x == 0) [2..n-1] 
