{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import Data.Traversable



data Expr = Expr (Exp Expr) 

data Exp s = I Int
           | Add s s
           | Sub s s
           | Mul s s
             deriving (Foldable, Functor, Traversable)

instance Num Expr where  
  (+) a b = Expr (Add a b)
  (-) a b = Expr (Sub a b)
  (*) a b = Expr (Mul a b)
  abs = undefined
  signum = undefined 
  fromInteger n = Expr (I (fromInteger n))

test :: Expr
test = 1 + 2 + 3 + 4 + 5 

adds :: Expr -> Int
adds (Expr e) = foldl isAdd 0 e
  where isAdd sum (Expr (Add _ _)) = sum + 1
        isAdd sum _ = sum 


             
