

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

-- fomrIntegral :: (Num b, Integral a) => a -> b
fromIntegralEx = fromIntegral 4 + 1.2

-- Syntax in Functions
errorExample = error "Crash the program"

-- Patterns to avoid repeating yourself
patternEx :: String -> String
patternEx all@(x:xs) = "The head of " ++ all ++ " is " ++ [x]




