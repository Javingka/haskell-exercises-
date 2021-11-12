--https://www.cis.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf

-- Exercise 1: Wholemeal programming
-- Reimplement each of the following functions in a more idiomatic
-- Haskell style. Use wholemeal programming practices, breaking each
-- function into a pipeline of incremental transformations to an entire
-- data structure. Name your functions fun1’ and fun2’ respectively.
-- 1. fun1 :: [Integer] -> Integer
--      fun1 [] = 1
--      fun1 (x:xs)
--          | even x = (x - 2) * fun1 xs
--          | otherwise = fun1 xs
-- 2. fun2 :: Integer -> Integer
--      fun2 1 = 0
--      fun2 n 
--          | even n = n + fun2 (n ‘div‘ 2)
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
