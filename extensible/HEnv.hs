{-# LANGUAGE UndecidableInstances,RankNTypes, KindSignatures, NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses, TypeOperators,FlexibleContexts  #-}

module HEnv where

infixr 5 :&
infixr 6 :->
data X (p :: * -> *) = X
data ((n :: *) :-> (v :: *)) (p :: * -> *)  = n :-> (p v)
data (a :& t) (p :: * -> *)  = (a p) :& (t p)

class Ord v => Var n v | n -> v where
  getVar :: n

class MMapOps (l :: (* -> *) -> *) where
  hmap :: (forall v. Ord v =>  p v -> q v) -> l p -> l q
  hzip :: (forall v. Ord v =>  p v -> q v -> z v) -> l p -> l q -> l z
  hfold :: (forall v. Ord v =>  p v -> a -> a) -> a ->  l p -> a
  hfoldbuild :: (forall v. Ord v =>  a -> p v -> (p v,a)) -> a ->  l p -> (l p, a)
  hfoldzip :: (forall v. Ord v => p v -> q v -> a -> a) -> a ->  l p -> l q -> a

instance MMapOps X where
  hmap _ X = X
  hzip _ X X = X
  hfold _ x _ = x
  hfoldbuild _ x X = (X,x)
  hfoldzip _ x _ _ = x

instance (Var n v, MMapOps t) => MMapOps (n :-> v :& t) where
  hmap f (n :-> h :& t) = n :-> (f h) :& hmap f t
  hzip f (n :-> hl :& tl) (_ :-> hr :& tr) = n :-> (f hl hr) :& hzip f tl tr
  hfold f x (n :-> hl :& tl) = f hl (hfold f x tl)
  hfoldbuild f x (n :-> hl :& t) = let (hl',x') = f x hl 
                                       (t',x'') = hfoldbuild f x' t
                                   in (n :-> hl' :& t', x'')
  hfoldzip f x (n :-> hl :& tl) (_ :-> hr :& tr) = f hl hr (hfoldzip f x tl tr)
 
class Build (x :: (* -> *) -> *) where
  build :: (forall v. p v) -> x p

instance (Var n v, Build t) => Build (n :-> v :& t) where
  build i = getVar :-> i :& build i

instance Build X where
  build i = X

class MMapConcat (l ::  (* -> *) -> *) (r ::  (* -> *) -> *) (x ::  (* -> *) -> *) | l r -> x where
  hconcat :: l p -> r p -> x p 

instance MMapConcat l X l where
  hconcat l X = l

instance MMapConcat X l l where
  hconcat X l = l

instance (Var n v, MMapConcat l r x) => MMapConcat (n :-> v :& l) r (n :-> v :& x) where
  hconcat (n :& t) r = n :& hconcat t r

class MMapRemove (l ::  (* -> *) -> *) (r ::  (* -> *) -> *) (x ::  (* -> *) -> *) | x l -> r where
  hremove :: x p -> l q -> r p

instance MMapRemove l X l where
  hremove _ _ = X

instance MMapRemove X l l where
  hremove l X = l

instance (Var n v, MMapRemove l r x) => MMapRemove (n :-> v :& l) r (n :-> v :& x) where
  hremove (_ :& tl) (_ :& tr) = hremove tl tr

class Has n v (l :: (* -> *) -> *) where
  hlookup :: n -> l p -> p v
  hmod :: n -> (p v -> p v) -> l p -> l p 

instance Var n v => Has n v ((n :-> v) :& t) where
  hlookup n (_ :-> v :& _) = v
  hmod n f (_ :-> v :& t) = n :-> f v :& t

instance Has n v t => Has n v (a :& t) where
  hlookup n (_ :& t) = hlookup n t
  hmod n f (a :& t) = a :& hmod n f t

