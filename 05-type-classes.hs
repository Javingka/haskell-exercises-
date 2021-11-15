import ExprT

-- https://www.cis.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf

{- 
=============================================================                    
    Exercise 1
=============================================================
-}
-- Write Version 1 of the calculator: an evaluator for ExprT, with the
-- signature
-- eval :: ExprT -> Integer
-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.


eval :: ExprT -> Integer
eval (Mul ( Add (Lit a) (Lit b) ) (Lit c) ) = ( a + b ) * c
eval (Add ( Mul (Lit a) (Lit b) ) (Lit c) ) = ( a * b ) + c
eval (Mul (Lit a) (Lit b) ) = a * b
eval (Add (Lit a) (Lit b) ) = a + b
-- eval Mul ( Add a b ) Lit c = a + b * c