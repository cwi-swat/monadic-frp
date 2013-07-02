{-# LANGUAGE TypeFamilies,NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses,GADTs, TypeOperators,FlexibleContexts  #-}

module DynMap where


import HMap
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Data.Set as Set
import Prelude hiding (lookup)

type family TypeOf a :: *

newtype ValOf a = Val (TypeOf a) 


data Predicate a where
  Any :: Predicate a
  Neq :: Ord a => a -> Predicate a
  DontCare :: Predicate a

instance Ord (Predicate a) where
  a `compare` b | cmpMajor /= EQ = cmpMajor
                | otherwise      = compareArgs a b
   where
   cmpMajor = toNum a `compare` toNum b
   compareArgs (Neq a) (Neq b) = a `compare` b
   toNum Any = 0
   toNum (Neq _) = 1
   toNum DontCare = 2

instance Eq (Predicate a) where
  a == b = a `compare` b == EQ


data PredOf n = n :? Predicate (TypeOf n)

instance Ord (PredOf n) where
  (_ :? p) `compare` (_ :? q) = p `compare` q

instance Eq (PredOf n) where a == b = a `compare` b == EQ

newtype PredsOf n = PredsOf (Set.Set (PredOf n))

unionPred (PredsOf a) (PredsOf b) = PredsOf (a `Set.union` b)


class Union a where
  union :: HList PredsOf a -> HList PredsOf a -> HList PredsOf a

instance Union Empty where
  union _ _ = X

instance (Ord h, Union t) => Union (Cons h t) where
  union (hl :& tl) (hr :& tr) = unionPred hl hr :& union tl tr

class Satisfies a where
  sat :: HList ValOf a -> HList PredsOf a -> Bool

instance Satisfies Empty where
  sat _ _ = False

instance Satisfies t => Satisfies (Cons h t) where
   sat (hl :& tl) (hr :& tr) = satSet hl hr || sat tl tr


sats _       (_ :? Any)      = True
sats (Val p) (_ :? (Neq q))  = p /= q
sats _       (_ :? DontCare) = False

satSet v (PredsOf s) = any (sats v) (Set.elems s)



data MouseMove = MouseMove deriving (Ord,Eq)
data MouseOver = MouseOver deriving (Ord,Eq)

type instance TypeOf MouseMove = Int 
type instance TypeOf MouseOver = Bool



instance HasDefault PredsOf where
  def = PredsOf Set.empty

padd :: PredOf n -> PredsOf n -> PredsOf n
padd a (PredsOf b) = PredsOf $ Set.insert a b

data React e a 
  = Done a
  | Await (HList PredsOf e) (HList ValOf e -> React e a)


instance Monad (React e) where
  return              = Done
  (Await e c)  >>= f  = Await e ((>>= f) . c)
  (Done v)     >>= f  = f v


first
  :: (Union e, Satisfies e) =>
     React e t -> React e t1 -> React e (React e t, React e t1)
first l r = case (l,r) of
  (Await el _, Await er _) -> 
      let  e    = el `union` er
           c b  = first (update l b) (update r b)
      in Await e c
  _ -> Done (l,r)

update :: Satisfies a => React a t -> HList ValOf a -> React a t
update (Await e c) b  | sat b e = update (c b) b
update r           _  = r

exper
  :: (HasDefaults ns, Has n ns) =>
     n -> PredOf n -> React ns (ValOf n)
exper n p = Await (modify (padd p) defs) (\s -> Done (lookup n s))

mm = exper (MouseMove :? Neq 3)

done (Done a) = Just a
done _        = Nothing

type RState ns e = (React ns e, HList ValOf ns)



newtype RStateM ns m i a = RStateM (RState ns i -> m (RState ns i, a))

instance (Satisfies ns, Monad m) => Monad (RStateM ns m i) where
  return a = RStateM (\x -> return (x,a))
  (RStateM f) >>= g = RStateM cont where
     cont x = do  (x',a) <- f x
                  let x'' = uprstate x'
                  let RStateM gi = g a 
                  gi x''
     uprstate (x,b) = (update x b, b)

get :: Monad m => RStateM ns m i (HList ValOf ns)
get = RStateM (\(r,vs) -> return ((r,vs),vs))

getR :: Monad m => RStateM ns m i (Maybe i)
getR = RStateM (\(r,vs) -> return ((r,vs), done r))

put :: Monad m => HList ValOf ns -> RStateM ns m i ()
put a = RStateM (\(r,_) -> return ((r,a),()))

{--
dynlet
  :: MonadReader (HList Assign t) m =>
     Assign a -> ReaderT (HList Assign (Cons a t)) m b -> m b
dynlet v a = do l <- ask
                let l' = v :& l
                runReaderT a l'

dynstart a = runReaderT a X 

give :: (Has b b1, MonadReader (HList Assign b1) m) => b -> m (TypeOf b)
give n = ask >>= (\s ->
         let _ := v = lookupG n s in
         return v)





test = dynlet (MouseMove := 3) $
         dynlet (MouseOver := False) $
           (give MouseOver)

--}

{--

toDynMap :: HList n a -> DynMap n
toDynMap X = M.empty
toDynMap (n := a :& t) = M.singleton n (toDyn a) `M.union` (toDynMap t)

dlets l a = do rest <- get 
               let s = toDynmap l `M.union` rest
               evalStateT s 


--}


