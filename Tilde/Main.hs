{-# LANGUAGE ScopedTypeVariables #-}
-- ghc --make Main.hs

module Main where

import Apa

test :: forall t e . (Do t, Num e) => Cepa e -> Apa t (Cepa e) 
test a = cFun x
 where
   x :: Bepa t e
   x = p a


q :: Apa A (Cepa Int)
q = test tCepa

q' :: Apa B (Cepa Int)
q' = test tCepa


tCepa :: Cepa Int
tCepa = Cepa 1 "hej"

main = putStrLn $ show q  ++ "\n" ++
                  show q'
