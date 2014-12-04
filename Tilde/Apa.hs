{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Apa where


data A
data B 

data Apa t a where
   Apa :: a -> Apa t a

instance Show a => Show (Apa t a) where
  show (Apa a) = show a 

-- Bepa and Cepa are the two payloads of Apas in this
-- example. The Cepa has a string that provides some helpful output. 
data Bepa t a = Bepa a
                deriving Show 
data Cepa a = Cepa a String
              deriving Show

-- A class that "generalises" the functionality
-- of class Do (below). C varies both dimensions
-- and thus abstracts away from the specific functions
-- in of Do. 

class C t a where
  cFun :: a e -> Apa t (Cepa e) 


instance Do t => C t Cepa where
  cFun = apaCepa

 -- The next instance of C is what causes problems

------------------------------------------------------------
-- VARY THIS PART
------------------------------------------------------------
  
-- Does not compile with this
--instance Do t => C t (Bepa t) where
--  cFun = apaBepa

-- Compiles with this 
instance (t ~ t1, Do t) => C t (Bepa t1) where
  cFun = apaBepa

------------------------------------------------------------
--
------------------------------------------------------------

-- Class do works on different t's,
-- But has specific functions for differing t bearers 
class Do t where
  apaCepa :: Cepa a -> Apa t (Cepa a) 
  apaBepa :: Bepa t a -> Apa t (Cepa a) 

-- There is an instance for every allowable t 
instance Do A where
  apaCepa (Cepa a _) = Apa (Cepa a "Cepa -> A")
  apaBepa (Bepa a) =  Apa (Cepa a "Bepa -> A") 

instance Do B where
  apaCepa (Cepa a _)= Apa (Cepa  a "Cepa -> B")
  apaBepa (Bepa a) = Apa (Cepa a "Bepa -> B") 


-- p is a converter from a Cepa to a Bepa.
-- Here a t is magically dreamt up (that needs to be
-- locked down by the programmer at some point)
p :: Cepa a -> Bepa t a
p (Cepa a _) = Bepa a

