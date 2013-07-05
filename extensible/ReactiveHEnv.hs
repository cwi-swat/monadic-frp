{-# LANGUAGE ViewPatterns,UndecidableInstances,RankNTypes, KindSignatures, NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses, TypeOperators,FlexibleContexts  #-}


module ReactiveHEnv where

import HEnv
import Data.Set 
import Prelude hiding (map)

data Predicate a = Any | Neq a | Eq a | DontCare deriving (Ord,Show,Eq)

data PredSet a = PredSet (Set (Predicate a))

sat v Any = True
sat v (Neq q) = v /= q
sat v (Eq a) = v == a
sat v DontCare  = False


satSet ::  Ord v =>  V v -> PredSet v -> Bool
satSet (V v) (PredSet s) = fold (||) False (map (sat v) s)

data V x = V x
onV f (V x) = V (f x)

class Satisfies l where
  satisfies :: l V -> l PredSet -> Bool

instance Satisfies X where
  satisfies X X = False

instance (Var n v, Ord v, Satisfies t) => Satisfies ((n :-> v) :& t) where
  satisfies ((vget -> v) :& tl) ((vget -> p) :& tr) = satSet v p || satisfies tl tr

class UnionPreds l where
  unionPreds :: l PredSet -> l PredSet -> l PredSet

instance UnionPreds X where
  unionPreds X X = X

instance (Var n v, Ord v, UnionPreds t) => UnionPreds ((n :-> v) :& t) where
  unionPreds (a :& tl) (b :& tr) = vzip punion a b :& unionPreds tl tr

punion (PredSet a) (PredSet b) = PredSet (a `union` b)




