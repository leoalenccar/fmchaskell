{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = undefined
head (x : xs) = x  

tail :: [a] -> [a]
tail [] = undefined
tail (x : xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = undefined
length (_ : xs) = length xs + 1

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
(++) [] xs = xs
(++) (x : xs) ys = x : (xs ++ ys) 

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc y (x : xs) = x : snoc y xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = undefined
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = undefined
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

take :: (Integral i) => i -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

drop :: (Integral i) => i -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x : xs) = drop (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile b (x : xs) =
  if b x
    then x : takeWhile b xs
    else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile b (x : xs) =
  if b x
    then dropWhile b xs
    else x : xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

init :: [a] -> [a]
init [] = undefined
init [x] = []
init (x : xs) = x : init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = inits (init xs) ++ [xs]

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) =
  let subs = subsequences xs
    in subs ++ map (x :) subs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any b (x : xs) = 
  if b x 
    then True 
    else any b xs

all :: (a -> Bool) -> [a] -> Bool
any _ [] = True
any b (x : xs) = 
  if b x 
    then all b xs 
    else False

and :: [Bool] -> Bool
and [] = True
and (x : xs) = 
  if x 
    then and xs 
    else False

or :: [Bool] -> Bool
or (x : xs) = 
  if x 
    then True 
    else or xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs : xss) = xs ++ concat xss

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x xs = any (== x) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys) =
  if x == y
    then True
    else elem' x

(!!) :: Integral i => [a] -> i -> a
(!!) [] _ = undefined
(!!) (x : xs) 0 = x
(!!) (x : xs) i =
  if i < 0 
    then undefined
    else xs !! (i - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter b (x : xs) =
  if b x
    then x : filter b xs
    else filter b xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

cycle :: [a] -> [a]
cycle [] = undefined
cycle xs  = xs ++ cycle xs

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: integral i => i -> a -> [a]
replicate n x = take n (repeat x)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) =
  if x == y
    then isPrefixOf xs ys
    else False

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf xs (y : ys) =
  if isPrefixOf xs (y : ys)
    then True
    else isInfixOf xs ys

isSuffixOf ::Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [xs] = xs
intercalate s (x : xs) = x ++ s ++ intercalate s xs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) =
  if elem x xs
    then nub xs
    else x : nub xs

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break b (x : xs) =
  if p x
    then ([], x : xs)
    else let (ys, zs) = break b xs
      in (x : ys, zs)

lines :: String -> [String]
lines [] = []
lines cs =
  let (line, rest) = break (== '\n) cs
  in case rest of
    [] -> [line]
    (_ : xs) => line : lines xs

words :: String -> [String]
words [] = []
words cs =
  let cs' = dropWhile (== ' ') cs
    (word, rest) = break (== ' ') cs'
  in case cs' of
    [] -> []
    _  -> word : words rest

unlines :: [String] -> String
unline [] = []
unline (x : xs) = x ++ ['\n] ++ unlines xs

unwords :: [String] -> String
unwords [] = []
unwords [x] = x
unwords (x : xs) = x ++ [' '] ++ unwords xs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs = map head xs : transpose (map tail xs)

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

