-- Importing modules
import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cube as Cube
import qualified Geometry.Cuboid as Cuboid

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

initsEx = inits [1,2,3,4]

tailsEx = tails [1,2,3,4]

isInfixOfEx = isInfixOf "smith" "joe smith"

isPrefixOfEx = isPrefixOf "joe" "joe smith"

isSuffixOfEx = isSuffixOf "bloggs" "joe bloggs"

elemEx = elem 5 [1,5,6]

notElemEx = notElem 5 [1,2,3]

-- Returns 2 tuple, first elem is items matching condition, second elem is items not matching condition
partitionEx = partition (>2) [1,2,3,4,5,6]

-- Finds first item matching perdicate, returns maybe value
-- Maybe values can either have a single value (Just something) or no value (Nothing)
findEx = find (>5) [1,5,6]

elemIndexEx = elemIndex 2 [1,2,3,4]

elemIndicesEx = elemIndices 2 [1,2,2,2,3,4]

findIndexEx = findIndex (==2) [1,2,3,4,2]

findIndicesEx = findIndices (==2) [1,2,2,3,4]

zip3Ex = zip3 [1,2] [3,4] [5,6]

zipWith3Ex = zipWith3 (\x y z -> x+y+z) [1,2] [3,4] [5,6]

linesEx = lines "joe\nsmith\nis\nhere"

unlinesEx = unlines ["joe", "smith"]

wordsEx = words "joe bloggs is not real"

unwordsEx = unwords ["joe", "bloggs"]

--Remove duplicates
nubEx = nub [1,1,2,3,4,4]

--Remove first occurance
deleteEx = delete 3 [1,2,3,4,5]

--Set difference
diffEx = [1..10] \\ [3,4,5]

--Set union
unionEx = union [1,2,3] [3,4,5]

--Set intersect
intersextEx = intersect [1,2,3,4] [3,4,5,6]

insertEx = insert 4 [1,2,3,4,6,7]

-- Some functions reserved for ints e.g. length have generic counterparts e.g. genericLength

groupByEx = groupBy (\x y -> (x > 0) == (y > 0)) [-1,2,-3,2,3,4,-5]

onEx = groupBy ((==) `on` (>0)) [1,2,3,-1,-2,1,2,-4]

sortByEx = sortBy (compare `on` length) [[1,2,3],[4,5,6,7],[9]]

--Data.Char

filterCharEx = all isAlphaNum "joe123"

--Chars fall in to categories which can be found using the 'generalCategory' function

genCatEx = generalCategory ' '

digitToIntEx = map digitToInt "3456"

ordEx = ord 'a'

chrEx = chr 97

--Data.Map

findKey :: (Eq k) => k ->  [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k then Just v else findKey key xs

findKeyFold key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- Takes associated list and returns a map
-- fromList :: (Ord k) => [(k, v)] -> Map.Map k v
-- Key must be of type Ord, as map uses an ordered tree
fromListEx = Map.fromList [(1,11),(2,12),(3,13)]

emptyEx = Map.empty

mapInsertEx = Map.insert 3 100 Map.empty

checkIfEmptyEx = Map.null Map.empty

sizeEx = Map.size $ Map.fromList [(1,2),(3,4)] 

singletonEx = Map.singleton 3 7

lookupEx = Map.lookup 3 $ Map.singleton 3 8

memberEx = Map.member 3 $ Map.fromList [(2,3), (3,4)]

mapMapEx = Map.map (*100) $ Map.singleton 2 5

mapFilterEx = Map.filter isUpper $ Map.fromList [(1,'a'), (2,'G')]

toListEx = Map.toList . Map.insert 7 8 $ Map.singleton 3 4

-- keys == map fst . Map.toList
keysEx = Map.keys $ Map.fromList [(1,2), (3,4)]

-- elems = map snd . Map.toList
elemsEx = Map.elems $ Map.fromList [(1,2), (3,4)]

-- fromListWith -> same as fromList but does not discard duplicates, uses function instead
fromListWithEx = Map.fromListWith max [(1,2), (1,5), (3,5), (3,7)]

-- insertWith -> uses function to determine what to do when duplicate found
insertWithEx = Map.insertWith (+) 3 100 $ Map.fromList [(3,10), (4,10)]

-- Data.Set -> ordered + unique
sharedEx = Set.intersection (Set.fromList "joe") (Set.fromList "moe")

-- Other funcs include difference, union, null size, member, empty, singleton, insert, delete

-- Set A is subset of set B, if B contains all elements in A
subsetOfEx = (Set.fromList "abc") `Set.isSubsetOf` (Set.fromList "abc")

-- Set A is proper subset of set B, if B contains all elements in A plus some more 
properSubsetOfEx = (Set.fromList "abc") `Set.isProperSubsetOf` (Set.fromList "xxabcxx")

setMapAndFilterEx = Set.map toUpper $ Set.filter isLower $ Set.fromList "abcDEF"

-- Following is faster than nub on large lists, but requires elements to be of Ord type. nub preserves order, setNub does not.
setNub xs = Set.toList $ Set.fromList xs


