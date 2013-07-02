{-# LANGUAGE TypeFamilies,NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses,GADTs, TypeOperators,FlexibleContexts  #-}


module HMap where

import Prelude hiding (lookup)




data Empty
data Cons h t 



data HList p a where
  X :: HList p Empty
  (:&) :: p a -> HList p t -> HList p (Cons a t)


class Has a b  where
  lookup :: a -> HList p b -> p a
  modify :: (p a -> p a) -> HList p b -> HList p b


instance Has n (Cons n t)  where
  lookup _ (v :& _) = v
  modify f (v :& t) = f v :& t

instance Has n t => Has n (Cons b t)  where
  lookup a (_ :& t) = lookup a t
  modify f (h :& t) = h :& (modify f t)

class HasDefault p where
  def :: p a

class HasDefaults a where
  defs :: HasDefault p => HList p a

instance HasDefaults Empty where
  defs = X

instance HasDefaults t => HasDefaults (Cons h t) where
  defs = def :& defs



