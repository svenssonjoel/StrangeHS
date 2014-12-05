{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- ghc --make Main.hs

module Main where

import Apa

#if 0 
test :: forall t e . (Do t, Num e) => Cepa e -> Apa t (Cepa e) 
test a = cFun x
 where
   x :: Bepa t e
   x = p a
#else 
test :: (Do t, Num e) => Cepa e -> Apa t (Cepa e) 
test a = cFun x
 where
   x = p a
#endif 

q :: Apa A (Cepa Int)
q = test tCepa

q' :: Apa B (Cepa Int)
q' = test tCepa


tCepa :: Cepa Int
tCepa = Cepa 1 "hej"

main = putStrLn $ show q  ++ "\n" ++
                  show q'
