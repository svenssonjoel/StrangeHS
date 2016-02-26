{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import Data.Traversable
 (Foldable, Functor, Traversable)

instance Num Expr where  
  (+) a b = Expr (Add a b)
  (-) a b = Expr (Sub a b)
  (*) a b = Expr (Mul a b)
  abs = undefined
  signum = undefined 
  fromInteger n = Expr (I (fromInteger n))


adds :: Expr -> Int
adds (Expr e) = foldl isAdd 0 e
  where isAdd sum (Expr (Add _ _)) = sum + 1
        isAdd sum _ = sum 



test :: Expr
test = 1 + 2 + 3 + 4 + 5

test2 :: Expr
test2 = 1 * 2 + 3 - 1 + 2 + 3 + 4 - 2




















foldExp :: (a -> Exp Expr -> a) -> a -> Exp Expr ->  a
foldExp f a e@(I i) = f a e
foldExp f a e@(Add (Expr e1) (Expr e2)) = f (foldExp f (foldExp f a e2) e1) e
foldExp f a e@(Sub (Expr e1) (Expr e2)) = f (foldExp f (foldExp f a e2) e1) e
foldExp f a e@(Mul (Expr e1) (Expr e2)) = f (foldExp f (foldExp f a e2) e1) e


adds_ :: Expr -> Int
adds_ (Expr e) = foldExp isAdd 0 e
  where isAdd sum (Add _ _) = sum + 1
        isAdd sum _ = sum 


             
