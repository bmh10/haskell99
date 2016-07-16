

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
