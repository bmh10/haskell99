-- Importing modules
import Data.List

-- GHCI
-- :m + Data.List

-- Import specific functions
import Data.List (nub, sort)

-- Import all except specific functions
import Data.List hiding (nub)

-- Ensure use of qualifier
import qualified Data.List as L

-- 1. Starting out
basic = succ 5 + min 5 6 + max 1 2 + 2 `div` 2

-- If then else
ifExample x = if x > 10 then 1 else 2

-- Definition
name = "Ben"

-- Concat operator
listConcat x y = x ++ y

-- Cons operator
cons :: a -> [a] -> [a]
cons x y = x:y

-- Index operator
index :: Int -> [a] -> a
index i xs = xs !! i

-- List operations
listOps xs  = head xs : tail xs
listOps' xs = init xs ++ [last xs]

listEmpty xs = length xs == 0 && null xs

listOps'' xs = reverse xs ++ take 2 xs ++ drop 2 xs

listMinMax xs = minimum xs : [maximum xs]

listSumProd xs = sum xs : [product xs]

listContains x xs = x `elem` xs

-- Ranges
range = [2,4..20]
revRange = [20,19..0]

rangeInf = take 5 [1,3..]
rangeInf' = take 10 $ cycle [1,2,3]
rangeInf'' = take 10 $ repeat 5
rangeInf''' = replicate 5 10

-- List Comprehension
listComp = [x | x <- [1..10], odd x]

-- Tuples
tuple = fst (1,2) + snd (3,4)
tupleZip = zip [1,2,3] [4,5,6]
tupleZipInf = zip [1..] "abc"

-- Typeclasses
-- Eq => can be compared
-- Ord => can be ordered (uses compare function)
-- Show => can be presented as a String (show function)
-- Read => can convert from String to another type (read function)
-- Enum => can be sequentially ordered (e.g. can be used in list ranges)
-- Bounded => has an upper and lower bound (functions: minBound, maxBound)
-- Num => can act like a number
-- Integral => whole numbers e.g. Int, Integer
-- Floating => floating point numbers e.g. Float, Double

-- fromIntegral :: (Num b, Integral a) => a -> b
fromIntegralEx = fromIntegral 4 + 1.2

-- Syntax in Functions
errorExample = error "Crash the program"

-- Patterns to avoid repeating yourself
patternEx :: String -> String
patternEx all@(x:xs) = "The head of " ++ all ++ " is " ++ [x]

-- Guards
guardEx a
 | a == 0 = 0
 | otherwise = 1

-- Where
whereEx name = n : lastName
  where (n:ns) = name
        lastName = " Smith"

-- Let
letEx name = let lastName = " Smith" in name ++ lastName

letEx2 = [func | x <- [1..10], let func = x*x]

-- Case expressions
caseEx xs = case xs of [] -> "Empty"
                       [x] -> "Singleton"
                       xs -> "Longer list"

-- Recursion
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ bigger
  where smaller = quicksort $ filter (<=x) xs
        bigger =  quicksort $ filter (>x)  xs 

-- Currying
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Higher order functions
zipWithEx = zipWith (+) [1,2,3] [4,5,6]

flipEx = (flip div) 2 10

mapEx = map fst [(1,2), (3,5)]

filterEx = filter (\x -> x > 3 && even x) [1..30]

sumOfOddSquaresUpto10000 = sum $ takeWhile (<10000)  $ filter odd $ map (^2) [1..]

foldEx = foldl (\acc x -> acc + x) 0 [1..10]
foldEx' = foldl (+) 0 [1..10]

foldrEx = foldr (\x acc -> x + acc) 0 [1..10]

scanEx = scanl (+) 0 [1..10]

-- Function composition
oddSquareSumCompositionEx = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- Data.List modules
intersperseEx = intersperse '.' "Banana"

intercalateEx = intercalate [0,0,0] [[1,2,3], [4,5,6]]

transposeEx = transpose [[1,2,3],[4,5,6]]

concatEx = concat ["john", "smith"]

concatMapEx = concatMap (replicate 3) [1..10]

andEx = and $ map (==4) [1,2,3,4]

orEx = or $ map (>4) [1,2,3,5]

anyEx = any (==4) [1,2,3,4]

allEx = all (>4) [1,2,3,5]

iterateEx = take 10 $ iterate (*2) 1

splitAtEx = splitAt 3 "foobar"

takeWhileEx = takeWhile (>1) [1,2,3,0,5]

dropWhileEx = dropWhile (<3) [1,2,2,3,4]

spanEx = span (<3) [1,2,3,4,5,6]

breakEx = break (==4) [1,2,3,4,5,6,7]

sortEx = sort [6,4,8,2,1]

groupEx = group [1,1,1,2,2,3,4,4]





