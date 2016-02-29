{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import Data.Traversable
       

data Expr = Expr (Exp Expr) 

data Exp s = Add s s
           | Sub s s
           | Mul s s
           | I Int 
  
       deriving (Foldable, Functor, Traversable)

instance Num Expr where  
  (+) a b = Expr (Add a b)
  (-) a b = Expr (Sub a b)
  (*) a b = Expr (Mul a b)
  abs = undefined
  signum = undefined 
  fromInteger n = Expr (I (fromInteger n))


adds :: Expr -> Int
adds (Expr e) = foldl isAdd 0 e
  where isAdd sum (Expr (Add s1 s2)) = sum + 1
        isAdd sum _ = sum 



test :: Expr
test = 1 + 2 + 3 + 4 + 5

test2 :: Expr
test2 = 1 * 2 + 3 - 1 + 2 + 3 + 4 - 2





















type Expr' = Mu Exp
data Mu f = In (f (Mu f))

fold :: Functor f => (f a -> a) -> Mu f -> a
fold f (In m) = f (fmap (fold f) m)

adds'' e = fold addHelp e
  where addHelp (Add s1 s2) = s1 + s2 + 1
        addHelp (Sub s1 s2) = s1 + s2
        addHelp (Mul s1 s2) = s1 + s2
        addHelp (I _)       = 0


foldExp :: (a -> Exp Expr -> a) -> a -> Exp Expr ->  a
foldExp f a e@(I i) = f a e
foldExp f a e@(Add (Expr e1) (Expr e2)) = f (foldExp f (foldExp f a e2) e1) e
foldExp f a e@(Sub (Expr e1) (Expr e2)) = f (foldExp f (foldExp f a e2) e1) e
foldExp f a e@(Mul (Expr e1) (Expr e2)) = f (foldExp f (foldExp f a e2) e1) e


adds_ :: Expr -> Int
adds_ (Expr e) = foldExp isAdd 0 e
  where isAdd sum (Add _ _) = sum + 1
        isAdd sum _ = sum 


             
