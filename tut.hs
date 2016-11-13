-- Importing modules
import Data.List
import Data.Function
import Data.Char
import Control.Monad
import System.IO
import System.IO.Error
import System.Directory
import System.Environment
import System.Random
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cube as Cube
import qualified Geometry.Cuboid as Cuboid
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S

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

-- Types & Typclasses
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- Record syntax
data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int,
                       height :: Float,
                       phoneNumber :: String,
                       flavour :: String
                     } deriving (Show)

data Car = Car { company :: String,
                 model :: String,
                 year :: Int
               } deriving (Eq, Show, Read)

carEx = Car {company="Ford", model="GT", year=2005}

-- Type params e.g. Maybe a = Just a | Nothing

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector a b c) (Vector i j k) = Vector (a+i) (b+j) (c+k)

vecMult :: (Num a) => Vector a -> a -> Vector a
vecMult (Vector a b c) n = Vector (a*n) (b*n) (c*n)

scalarMult :: (Num a) => Vector a -> Vector a -> a
scalarMult (Vector a b c) (Vector i j k) = a*i + b*j + c*k

-- Derived instances

readEx1 = read "Car {company=\"Ford\", model=\"Mustang\", year=1980}" :: Car

readEx2 = read "Just 't'" :: Maybe Char

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum) 

boundedEx = succ Monday

-- Type synonyms

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

-- Parameterized type synonyms
type AssocList k v = [(k, v)]

--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist."
    Just (state, code) -> if state /= Taken 
                        then Right code
                        else Left $ "Locker " ++ show lockerNumber ++ " is already taken."

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]

-- Recursive Data Structures

-- Defines right-associative function with binding priority
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
 | x == a = Node x left right
 | x < a  = Node a (treeInsert x left) right
 | x > a  = Node a left (treeInsert x right) 

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
 | x == a = True
 | x < a  = treeElem x left
 | x > a  = treeElem x right

nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums

-- Typeclasses 102

-- Class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x == y = not (x \= y)
--   x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

-- Define instance of Eq class manually instead of using 'deriving'
-- Minimal complete definition as /= is defined in terms of == within Eq
instance Eq TrafficLight where 
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False 

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

class YesNo a where yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where 
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _         = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- The Functor Typeclass

class MyFunctor f where 
  myfmap :: (a -> b) -> f a -> f b

-- fmap == map for lists
instance MyFunctor Maybe where
  myfmap f (Just a) = Just (f a)
  myfmap f Nothing  = Nothing

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

instance MyFunctor (Either a) where
  myfmap f (Right x) = Right (f x)
  myfmap f (Left x)  = Left x

-- Kinds => type of a type
-- e.g. :k Int (in GHCi)
-- * => concrete type


-- Input/Output
-- Compile: ghc --make tut
-- Run on fly: runhaskell tut.hs

main1 = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hi " ++ name)

main2 = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main2

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- putStrLn - for strings
-- print    - for other types

-- when - if true, returns IO action passed, else return ()
main3 = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main3

sequenceEx = do
  sequence $ map print [1,2,3] 

mapMEx = do
  mapM_ print [1,2,3,4]

foreverEx = forever $ do
  putStrLn "Input: "
  l <- getLine
  putStrLn $ map toUpper l

-- In Haskell 'return' converts a non-IO type into an IO type e.g. String -> IO String

mainWhen = do
  when (True) $ do
    putChar 'a'

mainSequence = do
  rs <- sequence [getLine, getLine]
  print rs

mainForM = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  

mainGetContents = do  
    contents <- getContents  
    putStr (map toUpper contents)

-- interact takes a String -> String function and return an IO action that takes an input, will run the function on it, and print out the result
mainInteract = interact $ unlines . filter ((<10) . length) . lines

mainPalindromeInteract = interact respondPalindromes
respondPalindromes = unlines . 
  map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome")  . lines
  where isPalindrome xs = xs == reverse xs 

mainFileRead = do
  handle   <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

mainWithFileEx = withFile "test.txt" ReadMode (\h -> do
                   contents <- hGetContents h
                   putStr contents)

-- This is how withFile could be defined
withFile' :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile' path mode func = do
  handle <- openFile path mode
  result <- func handle
  hClose handle
  return result

-- Common functions when working with handles: hGetLine, hPutStr, hPutStrLn, hGetChar

mainSimpleFileRead = do
  contents <- readFile "test.txt"
  putStr contents

mainSimpleFileWrite = do
  contents <- readFile "test.txt"
  writeFile "testCaps.txt" (map toUpper contents)

-- appendFile appends to end instead of overwriting

-- Possible buffer modes: NoBuffering, LineBuffering, BlockBuffering (Maybe Int) 
mainBufferEx = do
  withFile "test.txt" ReadMode (\handle -> do
    hSetBuffering handle $ BlockBuffering (Just 2048)
    contents <- hGetContents handle
    putStr contents)

-- hFlush can be used on a handle to flush the buffer

mainFileOpsEx = do        
    handle <- openFile "test.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
    let  numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "test.txt"  
    renameFile tempName "test.txt"

-- Command line args
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ] 

mainCmdLineEx = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  
 
add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName

-- Randomness
randInt = random (mkStdGen 100) :: (Int, StdGen)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
 let (c1, gen')   = random gen
     (c2, gen'')  = random gen'
     (c3, gen''') = random gen''
  in (c1, c2, c3)

manyRandInts = randoms (mkStdGen 100) :: [Int]

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (val, gen') = random gen in val:randoms' gen'

finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfValues, finalGen) = finiteRandoms (n-1) newGen
  in (value:restOfValues, finalGen)

randomRangeEx = randomR (1,6) (mkStdGen 100) :: (Int, StdGen)

randomsRangeEx = take 10 $ randomRs ('a', 'z') (mkStdGen 3) :: [Char] 

mainRandomEx = do     
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  
    putStrLn $ take 20 (randomRs ('a','z') gen')  

mainRandomGuess = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNum, gen') = randomR (1,10) gen :: (Int, StdGen)
  putStr "Which number from 1 to 10 am I thinking of? "
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if (randNum == number)
      then putStrLn "Correct!"
      else putStrLn $ "Incorrect, it was " ++ show randNum
    askForNumber gen'

-- Bytestrings
-- Like lists but each element is 1 byte and handles laziness differently
-- More efficient than lists for reading large files

-- Can be strict (no thunks/promises/laziness) or lazy
-- Lazy bytestrings are not as lazy as lists - list is split into 64K chunks. Each chunk only loaded into memory when needed.

-- pack - takes a list a makes it 'less lazy' by packing into 64K chunks
-- unpack - inverse
packEx = B.pack [99, 97, 110]

-- fromChunks - takes list of strict bytestrings and converts to lazy bytestring
-- toChunks - inverse
fromChunksEx = B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]

-- cons - places byte at beginning of bytestream. Lazy -> creates new chunk even if current chunk is not full
-- cons' - strict version. Only creates new chunk if current chunk is full

consEx = B.cons 85 $ B.pack [80..84]
cons'Ex = B.cons' 85 $ B.pack [80..84]

mainBytesteamFileCopy = do
  (fn1:fn2:_) <- getArgs
  copyFile' fn1 fn2

copyFile' :: FilePath -> FilePath -> IO ()
copyFile' src dest = do
  contents <- B.readFile src
  B.writeFile dest contents

-- Exceptions
-- Pure and IO functions can throw exceptions but must be caught in IO part of code

mainCheckFileExistsEx = do
  (filename:_) <- getArgs
  fileExists <- doesFileExist filename
  if fileExists
    then do contents <- readFile filename
            putStrLn $ "The file has " ++ (show $ length $ lines contents) ++ " lines"
    else do putStrLn "The file does not exist!"

main = do toTry `catchIOError` handler

toTry :: IO ()
toTry =  do (filename:_) <- getArgs
            contents <- readFile filename
            putStrLn $ "The file has " ++ (show $ length $ lines contents) ++ " lines"

handler :: IOError -> IO ()
handler e
 | isDoesNotExistError e = 
     case ioeGetFileName e of 
       Just path -> putStrLn $ "File does not exist at: " ++ path
       Nothing -> putStrLn $ "File does not exist at unknown location"
 | otherwise = ioError e

{- Other IOError predicates: 
    isAlreadyExistsError
    isDoesNotExistError
    isAlreadyInUseError
    isFullError
    isEOFError
    isIllegalOperation
    isPermissionError
    isUserError
-}

-- Full list of IOError attributes can be found here: https://downloads.haskell.org/~ghc/6.10.1/docs/html/libraries/base/System-IO-Error.html#3

-- Functionally Solving Problems

-- Reverse Polish Notation
solveRPN :: String -> Float
solveRPN = head . foldl calc [] . words 
  where calc (x:y:ys) "*" = (x*y):ys
        calc (x:y:ys) "+" = (x+y):ys 
        calc (x:y:ys) "-" = (x-y):ys 
        calc (x:y:ys) "/" = (x/y):ys 
        calc (x:y:ys) "^" = (x**y):ys 
        calc (x:xs)  "ln" = log x:xs 
        calc xs     "sum" = [sum xs]
        calc xs numString = read numString:xs

-- Heathrow to London

data Section = Section { getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

--optimalPath :: RoadSystem -> Path

roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)  
