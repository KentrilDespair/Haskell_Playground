
module Playground
where



min' :: (Ord a) => a -> a -> a
min' x y = if x <= y then x 
                     else y

min'' :: (Ord a) => a -> a -> a
min'' x y
    | x < y     = x
    | otherwise = y
          

max' :: (Ord a) => a -> a -> a
max' x y = if x <= y then y 
                     else x

max'' :: (Ord a) => a -> a -> a
max'' x y 
    | x > y     = x
    | otherwise = y

head' :: [a] -> a
head' [] = error "Head on empty list"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of []    -> error "Head on empty list"
                       (x:_) -> x

tail' :: [a] -> [a]
tail' (_:xs) = xs

last' :: [a] -> a
last' (x:[]) = x
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' (_:[]) = []
init' (x:xs) = x : init' xs

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

length'' :: (Num b) => [a] -> b
length'' xs = sum [1 | _ <- xs]

null' :: [a] -> Bool
null' [] = True
null' _ = False 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

take'' :: (Num i, Ord i) => i -> [a] -> [a]
take'' n all@(x:xs)
    | n <= 0 = []
    | null all == True = []
    | otherwise = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

maximum' :: (Ord a) => [a] -> a
maximum' (x:[]) = x
maximum' (x:xs) = max x (maximum' xs)

minimum' :: (Ord a) => [a] -> a
minimum' (x:[]) = x
minimum' (x:xs) = min x (minimum' xs)

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Maximum from an empty list"
maximum'' [x] = x
maximum'' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum'' xs

minimum'' :: (Ord a) => [a] -> a
minimum'' [] = error "Minimum from an empty list"
minimum'' [x] = x
minimum'' (x:xs)
    | x < minTail = x
    | otherwise = minTail
    where minTail = minimum'' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (\acc x -> acc + x) 0

product' :: (Num a) => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' p (x:xs) = if p == x then True
                           else elem' p xs

-- ----------------------------------------------------------------------------
-- Functions that produce inifinite lists
cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

repeat' :: a -> [a]
repeat' x = x : repeat' x

replicate' :: Int -> a -> [a]
replicate' n p
    | n <= 0 = []
    | otherwise = p : replicate' (n-1) p 

fst' :: (a,b) -> a
fst' (x,_) = x

snd' :: (a,b) -> b
snd' (_,x) = x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- ----------------------------------------------------------------------------
-- Custom

-- TODO only ASCII
upperList = ['A'..'Z']
lowerList = ['a'..'z']

removeNonUppercase :: String -> String
removeNonUppercase s = [x | x <- s, x `elem` upperList]

isLower :: Char -> Bool
--isLower c = c `elem` lowerList
isLower c = c >= 'a' && c <= 'z'

isUpper :: Char -> Bool
--isUpper c = c `elem` upperList
isUpper c = c >= 'A' && c <= 'Z'

elemIndex :: (Eq a) => a -> [a] -> Int 
elemIndex _ [] = 0
elemIndex p (x:xs) = if p == x then 0
                               else 1 + elemIndex p xs

toLower :: Char -> Char 
toLower c = if isUpper c then lowerList !! elemIndex c upperList
                         else c

toUpper :: Char -> Char 
toUpper c = if isLower c then upperList !! elemIndex c lowerList 
                         else c

strToLower :: String -> String
strToLower s = map toLower s

strToUpper :: String -> String
strToUpper s = map toUpper s

--------------------------------------------------------------------------------
-- Syntax in functions
factorial :: (Integral a) => a -> a 
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: (Integral a) => a -> a 
factorial' n = product [1..n]

--------------------------------------------------------------------------------
-- Vectors

-- TODO Vector datatype
addVec :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Fst and snd for Triples
first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

compare' :: (Ord a) => a -> a -> Ordering
compare' x y
    | x < y     = LT
    | x == y    = EQ
    | otherwise = GT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname


-- fibonacci and fast
fib :: Int -> Int
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

fibFast :: Int -> Integer
fibFast n
    | n == 0 = 0
    | n == 1 = 1
    | n == 2 = 1
    | otherwise = f 0 1 0
    where f n_2 n_1 i = if i == n then n_2
                    else f n_1 (n_2 + n_1) (i+1)


-- quick sort
--  a sorted list is a list that has all the values smaller than 
--  (or equal to) the head of the list in front (and those values 
--  are sorted), then comes the head of the list in the middle and 
--  then come all the values that are bigger than the head 
--  (they're also sorted

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort bigger
    where smaller = [i | i <- xs, i <= x]
          bigger = [k | k <- xs, k > x]

--------------------------------------------------------------------------------
-- High order functions

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x then x : filter' f xs
                   else filter' f xs

-- Find the largest number under 100,000 that's divisible by 3829
largestDivisble :: (Integral a) => a
largestDivisble = head $ filter d [99999,99998..]
    where d x = x `mod` 3829 == 0


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x then x : takeWhile' f xs
                      else []

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs) 

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "foldl1 empty list"
foldl1' _ [x] = x
foldl1' f (x:xs) = fldl f x xs
    where fldl g acc l = if null l then acc
                         else fldl g (g acc (head l)) (tail l)

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ acc [] = [acc]
scanl' f acc (x:xs) = acc : scanl' f (f acc x) xs



