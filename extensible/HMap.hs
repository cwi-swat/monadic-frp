{-# LANGUAGE NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses, TypeOperators,FlexibleContexts  #-}


module HMap where

import Prelude hiding (map)
import Debug.Trace
import Data.Set

infixr 5 :&
infixr 6 :=
data X = X
data h :& t = h :& t
data n := v = V v

data Predicate a = Any | Neq a | Leq a | DontCare deriving (Ord,Show,Eq)

sat v Any = True
sat v (Neq q) = v /= q
sat v (Leq a) = v <= a
sat v DontCare  = False


satSet v s = fold (||) False (map (sat v) s)

class Ord v => Var n v | n -> v

class Has n v t where
  hlookup :: n -> t -> v
  hmod    :: n -> (v -> v) -> t -> t

instance Var n v =>  Has n v ((n := v) :& t) where
  hlookup n (V v :& _) = v
  hmod n f (V v :& t) = V (f v) :& t

instance Has n v t => Has n v (x :& t) where
  hlookup n (_ :& t) = hlookup n t
  hmod n f (x :& t) = x :& hmod n f t

type PredSet a = Set (Predicate a)

class HasPred n v t where
  addPred :: n -> Predicate v -> t -> t
  getPred :: n -> t -> PredSet v

instance Var n v => HasPred n v ((n := PredSet v) :& t) where
    addPred n p (V s :& t) = V (insert p s) :& t
    getPred n (V s :& _) = s

instance HasPred n v t => HasPred n v (x :& t) where
    addPred n p (x :& t) = x :& addPred n p t
    getPred n ( _ :& t) = getPred n t

class PredList x where
  noPreds :: x
  hunion :: x -> x -> x

instance PredList X where
  noPreds = X
  hunion _ _ = X

instance (Var n v, PredList t) => PredList ((n := PredSet v) :& t) where
  noPreds = V empty :& noPreds
  hunion (V p :& l) (V q :& r) = V (p `union` q) :& hunion l r

class Satisfies a b where
  satisfies :: a -> b -> Bool

instance Satisfies X X where
  satisfies _ _  = False

instance (Var n v, Satisfies l r) => Satisfies ((n := v) :& l) ((n := PredSet v) :& r) where
   satisfies (V v :& l) (V p :& r) = satSet v p || satisfies l r




