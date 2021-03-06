import Data.List ((\\))
-- import Data.ByteString.Lazy.Builder.ASCII (integerDec)
--https://www.cis.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf

{- 
=============================================================                    
    EXERCISE 1
=============================================================
-}

-- Exercise 1: Wholemeal programming
-- Reimplement each of the following functions in a more idiomatic
-- Haskell style. Use wholemeal programming practices, breaking each
-- function into a pipeline of incremental transformations to an entire
-- data structure. Name your functions fun1â and fun2â respectively.
-- 1. fun1 :: [Integer] -> Integer
--      fun1 [] = 1
--      fun1 (x:xs)
--          | even x = (x - 2) * fun1 xs
--          | otherwise = fun1 xs
-- 2. fun2 :: Integer -> Integer
--      fun2 1 = 0
--      fun2 n 
--          | even n = n + fun2 (n âdivâ 2)
--          | otherwise = fun2 (3 * n + 1)
--
-- Hint: For this problem you may wish to use the functions iterate
-- and takeWhile. Look them up in the Prelude documentation to see
-- what they do.

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer]-> Integer
fun1' xs = product $ map( \ x -> if even x then x - 2 else 1 ) xs


--

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' val = sum $ filter even (takeWhile (>1) $ iterate (\x -> if even x then x `div` 2 else 3 * x + 1 ) val )

{- 
=============================================================                    
    Exercise 2: Folding with trees
=============================================================
-}

-- Recall the definition of a binary tree data structure. The height of http://en.wikipedia.org/wiki/
-- Binary_tree a binary tree is the length of a path from the root to the deepest
-- node. For example, the height of a tree with a single node is 0; the
-- height of a tree with three nodes, whose root has two children, is 1;
-- and so on. A binary tree is balanced if the height of its left and right
-- subtrees differ by no more than 1, and its left and right subtrees are
-- also balanced.
-- You should use the following data structure to represent binary
-- trees. Note that each node stores an extra Integer representing the
-- height at that node.

data BTree a =
    Leaf
    | Node Integer (BTree a) a (BTree a)
    deriving (Show, Eq)

-- For this exercise, write a function
--     foldTree :: [a] -> Tree a
--     foldTree = ...
-- which generates a balanced binary tree from a list of values using
-- foldr.
-- For example, one sample output might be the following, also visualized at right:

foldTree :: [a] -> BTree a
foldTree = foldr insert Leaf
    where
        insert :: a -> BTree a -> BTree a
        insert value Leaf = Node 0 Leaf value Leaf
        insert value (Node h left val right)
            | lH > rH = makeNode val left (insert value right) -- left val (insert value right)
            | otherwise = makeNode val (insert value left) right
            where
                lH = getHeight left
                rH = getHeight right

        makeNode :: a -> BTree a -> BTree a -> BTree a
        makeNode val left right = Node (1 + max (getHeight left) (getHeight right) ) left val right

        getHeight :: BTree a -> Integer
        getHeight Leaf = -1
        getHeight (Node h _ _ _ ) = h

foldTreeB :: [a] -> BTree a
foldTreeB = foldr insert Leaf
    where
        insert :: a -> BTree a -> BTree a
        insert value Leaf = Node 0 Leaf value Leaf
        insert value (Node h left val right)
            | lH > rH = Node (1 + max (getHeight left) (getHeight right) ) left val (insert value right) -- left val (insert value right)
            | otherwise =  Node (1 + max (getHeight left) (getHeight right) )  (insert value left) val right
            where
                lH = getHeight left
                rH = getHeight right

        getHeight :: BTree a -> Integer
        getHeight Leaf = -1
        getHeight (Node h _ _ _ ) = h

-- Why foldTree works and not fordTreeB? what difference makes the facto to include an expression as Function or not?

{-
Node 2 
    (Node 3 
        (Node 1 (Node 1 Leaf 'e' Leaf) 
        'c' 
        (Node 1 (Node 1 Leaf 'e' Leaf) 'd' Leaf)) 'b' (Node 1 (Node 1 Leaf 'e' Leaf) 'd' Leaf)) 'a' (Node 1 (Node 1 Leaf 'e' Leaf) 'd' Leaf)
-}

{- 
=============================================================                    
    Exercise 3: More folds!
=============================================================
-}


-- 1. Implement a function
--      xor :: [Bool] -> Bool
-- which returns True if and only if there are an odd number of True
-- values contained in the input list. It does not matter how many
-- False values the input list contains. For example,
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
-- Your solution must be implemented using a fold.

xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc ) False

-- class solution:
xorB :: [Bool] -> Bool
xorB = odd . foldr (\x acc -> if x then acc + 1 else acc) 0


-- 2. Implement map as a fold. That is, complete the definition
-- mapâ :: (a -> b) -> [a] -> [b]
-- mapâ f = foldr ...
-- in such a way that mapâ behaves identically to the standard map
-- function

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ( \x acc -> f x : acc) []

{- 
=============================================================                    
    Exercise 4: Finding primes
=============================================================
-}


-- Read about the Sieve of Sundaram. Implement the algorithm us- http://en.wikipedia.org/wiki/Sieve_
-- of_Sundaram ing function composition. Given an integer n, your function should
-- generate all the odd prime numbers up to 2n + 2.
--      sieveSundaram :: Integer -> [Integer]
--      sieveSundaram = ...

-- To give you some help, below is a function to compute the Cartesian product of two lists. This is similar to zip, but it produces all
-- possible pairs instead of matching up the list elements. For example,
-- cartProd [1,2] [âaâ,âbâ] == [(1,âaâ),(1,âbâ),(2,âaâ),(2,âbâ)]
-- Itâs written using a list comprehension, which we havenât talked about
-- in class (but feel free to research them).

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = removeDoubleAndIncrement $ cartProd l l
    where 
        l = [1..n]
        removeDoubleAndIncrement :: [(Integer,Integer)] -> [Integer]
        removeDoubleAndIncrement xss = map (\x -> 2 * x + 1) (firstRemoval xss)
            where
                firstRemoval :: [(Integer,Integer)] -> [Integer] 
                firstRemoval xss = [1..n] \\ map ( \(a,b) -> a + b + 2 * a * b ) xss

-- class solution (import Data.List ((\\)) should be included for list Difference):
sieveSundaramB :: Integer -> [Integer]
sieveSundaramB n = map (\x->2*x+1) $ ([1..n] \\) $ map (\(i,j) ->i+j+2*i*j) $ cartProd [1..n] [1..n]


-- q = 2(i + j + 2ij) + 1