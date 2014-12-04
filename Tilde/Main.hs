
-- ghc --make Main.hs

module Main where

import Apa

test :: (Do t, Num a) => Cepa a -> Apa t (Cepa a) 
test a = cFun $ p a


q :: Apa A (Cepa Int)
q = test tCepa

q' :: Apa B (Cepa Int)
q' = test tCepa


tCepa :: Cepa Int
tCepa = Cepa 1 "hej"

main = putStrLn $ show q  ++ "\n" ++
                  show q'
