{-# LANGUAGE ViewPatterns,UndecidableInstances,RankNTypes, KindSignatures, NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses, TypeOperators,FlexibleContexts  #-}

module HEnv  where



infixr 5 :&
infixr 6 :->
data X (p :: * -> *) = X
data ((n :: *) :-> (v :: *)) (p :: * -> *)  = N (p v) | n :-> (p v)
data (a :& t) (p :: * -> *)  = (a p) :& (t p)

vget :: (n :-> v) p -> p v
vget (N v) = v
vget (n :-> v) = v

vmod f (N v)  = N (f v)
vmod f (n :-> v) = n :-> (f v)

vzip f l r = inject l r $ f (vget l) (vget r)
  where inject (n :-> _) _ = (n :->)
        inject _ (n :-> _) = (n :->)
        inject _ _ = N
        

class Var n v | n -> v

class Build (x :: (* -> *) -> *) where
  build :: (forall v. p v) -> x p

instance (Var n v, Build t) => Build (n :-> v :& t) where
  build i = N i :& build i

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
  hlookup n (v :& _) = vget v
  hmod n f (v :& t) =  vmod f v :& t

instance Has n v t => Has n v (a :& t) where
  hlookup n (_ :& t) = hlookup n t
  hmod n f (a :& t) = a :& hmod n f t

