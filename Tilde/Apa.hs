{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

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
  cFun :: forall e . a e -> Apa t (Cepa e) 


instance Do t => C t Cepa where
  cFun = apaCepa

 -- The next instance of C is what causes problems

------------------------------------------------------------
-- VARY THIS PART
------------------------------------------------------------

#if 0
-- Does not compile with this
instance Do t => C t (Bepa t) where
  cFun = apaBepa

-- Starting to see a difference here.
-- If the instance is defined as above, you can also
-- have this instance (below):
instance C A (Bepa B) where
  cFun = undefined 

-- but with the other definition of the instance (below)
-- the C A (Bepa B) instance is a conflict (overlapping instances) 
  

#else 
-- Compiles with this 
instance (t ~ t1, Do t) => C t (Bepa t1) where
  cFun = apaBepa

-- conflicting instance (overlapping!) 
--instance C A (Bepa B) where
--  cFun = undefined 

#endif

{- Questions:
  #1 does (t ~ t1) above read "it is enough to know
     what one of t and t1 is to decide"
  #2 if (t ~ t1) reads "if t is equal to t1 then this is instance is applicable",
     Then I dont see how this works.

  Answer:
   # The key to this is that the typechecker matches
     against the "head" of the instance (C t Bepa t1)
     not the constraint (t ~ t1).
     So that means things like C A (Bepa B) will match,
     but lead to a type error because of the (t ~ t1) constraint,
     rather than a "no instance for X " error.

  # Left to be answered is what happens in the "test" function
    in module Main. It seems to mysteriously making a decision
    about the type of "x" without proper guidance, is this because
    the typechecker nows that test/x will only pass typechecking
    if the t parameters match up ? 

-} 


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

