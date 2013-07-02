{-# LANGUAGE TupleSections, ViewPatterns, TypeFamilies,NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses,GADTs, TypeOperators,FlexibleContexts  #-}

module Reactive where

import HMap
import Data.Maybe
import Data.Set hiding (map,filter,foldl)
import Prelude hiding (lookup,null,map,filter,filter,until,repeat,cycle,scanl,span,break,either,foldl,mod,all)
import Control.Monad.Trans
import Debug.Trace

type family TypeOf a :: *

mod' :: Has n e =>  n -> (ValOf n -> ValOf n) -> HList ValOf e -> HList ValOf e
mod' n f l = modify f l

modv n f l = mod' n (\(Val x) -> Val (f x)) l

newtype ValOf a = Val (TypeOf a) 


data Predicate a where
  Any :: Predicate a
  Neq :: Ord a => a -> Predicate a
  Leq :: Ord a => a -> Predicate a
  DontCare :: Predicate a

instance Ord (Predicate a) where
  a `compare` b | cmpMajor /= EQ = cmpMajor
                | otherwise      = compareArgs a b
   where
   cmpMajor = toNum a `compare` toNum b
   compareArgs (Neq a) (Neq b) = a `compare` b
   compareArgs (Leq a) (Leq b) = a `compare` b
   toNum Any = 0
   toNum (Neq _) = 1
   toNum (Leq _) = 2
   toNum DontCare = 3

instance Eq (Predicate a) where
  a == b = a `compare` b == EQ


data PredOf n = n :? Predicate (TypeOf n)

instance Ord (PredOf n) where
  (_ :? p) `compare` (_ :? q) = p `compare` q

instance Eq (PredOf n) where a == b = a `compare` b == EQ

newtype PredsOf n = PredsOf (Set (PredOf n))

unionPred (PredsOf a) (PredsOf b) = PredsOf (a `union` b)


class Union a where
  unionps :: HList PredsOf a -> HList PredsOf a -> HList PredsOf a

instance Union Empty where
  unionps _ _ = X

instance Union t => Union (Cons h t) where
  unionps (hl :& tl) (hr :& tr) = unionPred hl hr :& unionps tl tr

class Satisfies a where
  sat :: HList ValOf a -> HList PredsOf a -> Bool

instance Satisfies Empty where
  sat _ _ = False

instance Satisfies t => Satisfies (Cons h t) where
   sat (hl :& tl) (hr :& tr) = satSet hl hr || sat tl tr


sats _       (_ :? Any)      = True
sats (Val p) (_ :? (Neq q))  = p /= q
sats (Val p) (_ :? (Leq q))  = p <= q
sats _       (_ :? DontCare) = False

satSet v (PredsOf s) = any (sats v) (elems s)





instance HasDefault PredsOf where
  def = PredsOf empty

padd :: PredOf n -> PredsOf n -> PredsOf n
padd a (PredsOf b) = PredsOf $ insert a b

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
      let  e    = el `unionps` er
           c b  = first (update l b) (update r b)
      in Await e c
  _ -> Done (l,r)

update :: Satisfies a => React a t -> HList ValOf a -> React a t
update (Await e c) b  | sat b e = trace "upNow" $update (c b) b
update r           _  = trace "Updone" r

exper'
  :: (HasDefaults ns, Has n ns) =>
     PredOf n -> React ns (ValOf n)
exper' (n :? p) = Await (modify (padd (n :? p)) defs) (\s -> Done (lookup n s))

exper p = exper'  p >>= return . dropVal
dropVal (Val v) = v

interpret :: Monad m =>
     (HList PredsOf ns -> m (HList ValOf ns)) -> 
     React ns a -> m a
interpret p (Done a)     = return a
interpret p (Await e c)  = p e >>= interpret p . c

-- | A signal computation is a reactive computation of an initialized signal
newtype  Sig      e a b     =  Sig (React e (ISig e a b)) 
-- | An initialized signal 
data     ISig     e a b     =  a :| (Sig e a b) 
                            |  End b

interpretSig
  :: Monad m =>
     (HList PredsOf ns -> m (HList ValOf ns))
     -> (t -> m a) -> Sig ns t b -> m b
interpretSig p d (Sig s) = 
  do  l <- interpret p s
      case l of
        h :| t  ->  d h >> interpretSig p d t
        End a   -> return a

instance Monad (Sig e a) where
  return a = emitAll (End a)
  (Sig l) >>= f = Sig (l >>= ib)
   where  ib (h :| t)  = return (h :| (t >>= f))
          ib (End a)   = let Sig x = f a in x

instance Monad (ISig e a) where
  return = End
  (End a)   >>= f = f a
  (h :| t)  >>= f = h :| (t >>= emitAll . f)

-- * Repetition
-- | Repeat the given reactive computation indefinitely, each time emitting its result.
repeat x = xs where xs = Sig (fmap (:| xs) x )
-- | Repeat the given signal computation indefinitely, each time emitting its initialized signal result.
spawn (Sig l) = repeat l

-- * Transformation
-- | Transform the emmited values of a signal computation by applying the function to each of them. 
map   f (Sig l)   = Sig (fmap (imap f) l) 
-- | Transform the emmited values of an initialized signal computation by applying the function to each of them. 
imap  f (h :| t)  = f h :| map f t
imap  f (End a)   = End a

-- | The list function scanl is similar to foldl, but returns a list of successive reduced values instead of a single value. 
-- the signal variant works analogously.
scanl   f i l        = emitAll (iscanl f i l)
iscanl  f i (Sig l)  = i :| (waitFor l >>= lsl)
   where  lsl (h :| t)  = scanl f (f i h) t
          lsl (End a)   = return a
-- | Run the signal computation as long as the given predicate does not hold on the emitted values. Once a value is emmited on which the predicate holds, the rest of the signal computation is returned.
break f (Sig l) = Sig (fmap (ibreak f) l)
ibreak f (h :| t)  | f h        = return (h :| t)
                   | otherwise  = h :| break f t
ibreak f (End a)                = return (End a)

-- | |foldl| on signal computations behaves the same as waiting for the signal computation to end and then applying the 'fold' on the list of emitted values.
foldl :: (a -> b -> a) -> a -> Sig e b r -> React e a
foldl   f i (Sig l)   = l >>= ifoldl f i
ifoldl :: (a -> b -> a) -> a -> ISig e b r -> React e a
ifoldl  f i (h :| t)  = foldl f (f i h) t
ifoldl  f i (End a)   = return i

-- | Find the first emmited value on which the predicate hold.
find f l = fmap icur (res (break f l))

-- * Parallel composition
-- | Sample the form of the signal computation at the time the reactive computation completes
l `at` a = fmap (cur . fst) (res (l `until` a))

-- | Run the signal computation until the reactive computation completes, and return the new state of both computations.
until (Sig l) a = waitFor (first l a) >>= un where
  un (Done l,a)  = do  (l,a) <- emitAll (l `iuntil` a)
                       return (emitAll l, a)
  un (l,a)       = return (Sig l,a) 
iuntil (End l)       a = End (End l,a)
iuntil (h :| Sig t)  a = h :| Sig (fmap cont (first t a)) 
  where  cont (Done l,a) = l `iuntil` a
         cont (t,Done a) = End (h :| Sig t, Done a)

-- | Apply the values from the second signal computation to the values from the first signal computation over time. When one ends, the new state of both is returned.
l <*> r = do  (l,r) <- waitFor (bothStart l r)
              emitAll (imap (\(f,a) -> f a) (pairs l r))

-- | Wait for both signal computation to become initialized, and then return both their initizialized signals.
bothStart l (Sig r) =  do   (Sig l,r)  <- res (     l  `until` r)
                            (Sig r,l)  <- res (Sig  r  `until` l)
                            return (done' l, done' r)
-- | Emitted the pairs of the emitted values from both signal computations over time.  When one ends, the new state of both is returned.
pairs (End a)    b           = End (End a,b)
pairs a          (End b)     = End (a,End b)
pairs (hl :| Sig tl) (hr :| Sig tr)  = (hl,hr) :| tail 
  where  tail = Sig (fmap cont (first tl tr))
         cont (tl,tr) = pairs (lup hl tl) (lup hr tr)
         lup _ (Done l)  = l; lup h t = h :| Sig t

-- | Sample the former signal computation each time the later emits a value.
indexBy ::  (Satisfies e, Union e) => Sig e a l -> Sig e b r 
            -> Sig e a ()
l `indexBy` (Sig r) = 
 do  (Sig l,r) <- waitFor (res (l `until` r))
     case (l,r) of
       (_,Done (End _))  -> return ()
       (Done l, r)       -> l `iindexBy` Sig r
       (l,Done (_:| r))  -> Sig l `indexBy` r


l  `iindexBy` (Sig r)  = 
  do  (l,r) <- waitFor (ires (l `iuntil` r))
      case (l,r) of
       (hl :| tl, Done (hr :| tr)) -> emit hl >> (hl :| tl) `iindexBy` tr
       _                           -> return ()

-- * Conversion
-- | Convert a initialized signal to a signal computation
emitAll    = Sig . Done       
-- | Emit a single value in the signal computation mondad
emit a = emitAll (a :| return ()) 
-- | A signal that alway has the given form.
pure a     = emit a >> hold   
-- | Convert a reactive computation to a signal computation.
waitFor a  = Sig (fmap End a)
-- | The reactive computation that never completes.
hold       = waitFor never 

never = Await defs undefined

-- | Convert the result of a signal computation to a reactive computation.
res (Sig l)    = l >>= ires 
-- | Convert the result of an initialized signal a reactive computation.
ires (_ :| t)  = res t; ires (End a)   = Done a

instance Functor (React e) where
  fmap f a = a >>= return . f
-- | Return the result of a reactive computation if it is done
done (Done a)              = trace "Done!" $ Just a  ; done _  = trace "not done" Nothing
-- | Give the current value of a signal computation, if any.
cur (Sig (Done (h :| _)))  = Just h  ; cur _   = Nothing

-- | the head of an initalized signal, if any.
icur (h :| t) = Just h
icur (End _)  = Nothing 

-- | Version of 'done' that throws an error if it the result is not done.
done' = fromJust . done


-- * Dynamic lists

-- | Cons the values from the first signal computation to the values form the latter signal computation over time.
cons :: (Satisfies e, Union e) =>  ISig e a l -> ISig e [a] r 
                  -> ISig e [a] ()
cons h t = do  (h,t) <- imap (uncurry (:)) (pairs h t)
               imap (: []) h
               t
               return ()
-- | Run the initialized signals from the given signal computation in parallel, and emit the lists of the current form of all alive initialized signals.
parList x = emitAll (iparList x)

iparList :: (HasDefaults e, Satisfies e, Union e) => Sig e (ISig e a l) r -> ISig e [a] ()

iparList l = rl ([] :| hold) l >> return () where
  rl t (Sig es) = do  (t,es) <- t `iuntil` es
                      case es of
                        Done (e :| es)  -> rl (cons e t) es
                        _               -> t 
                        


type RState ns e = (React ns e, HList ValOf ns)

newtype RStateM ns i m a = RStateM (RState ns i -> m (Either i (RState ns i, a)))

instance (Satisfies ns, Monad m) => Monad (RStateM ns i m) where
  return a = RStateM (\x -> return (Right (x,a)))
  (RStateM f) >>= g = RStateM cont where
     cont x = do  res <- f x
                  case res of
                    Left i -> return (Left i)
                    Right ((r,v),a) ->
                     let r' = update r v
                         RStateM gi = g a 
                     in case done r' of
                        Just a -> return (Left a)
                        Nothing -> gi (r',v)

instance MonadTrans (RStateM ns i)  where
  lift a = RStateM (\x -> a >>= return . Right . (x,))


getr :: Monad m => RStateM ns i m (HList ValOf ns)
getr = RStateM (\(r,vs) -> return (Right ((r,vs),vs)))

getPredsr = RStateM (\(r,vs) -> return (Right ((r,vs),preds r)))
  where preds (Await x _) = x


giver n = getr >>= (return . lookup n)


putr :: Monad m => HList ValOf ns -> RStateM ns i m ()
putr a = RStateM (\(r,_) -> return (Right ((r,a),())))

runStateM s r (RStateM f) = f (r,s)

type SState m ns e r = (e -> m (), Sig ns e r, HList ValOf ns)

newtype SStateM ns e r m a = SStateM (SState m ns e r -> m (Either r (SState m ns e r, a)))

instance (Satisfies ns, Monad m) => Monad (SStateM ns p i m) where
  return a = SStateM (\x -> return (Right (x,a)))
  (SStateM f) >>= g = SStateM cont where
     cont x = do  res <- f x
                  case res of
                    Left a -> return (Left a)
                    Right ((f,Sig r,v),a) ->
                     let r' = update r v
                         SStateM gi = g a 
                     in case done r' of
                        Just (End a)-> return (Left a)
                        _ -> do r'' <- maybeProc f (Sig r')
                                gi (f,r'',v)
     maybeProc f (Sig (done -> Just (h :| t))) = trace "maybe not done" $ f h >> maybeProc f t
     maybeProc f r = return r

instance MonadTrans (SStateM ns e r)  where
  lift a = SStateM (\x -> a >>= (trace "lifted" $ return . Right . (x,)))

sget :: Monad m => SStateM ns e r m (HList ValOf ns)
sget = SStateM (\(f,r,vs) -> return (Right ((f,r,vs),vs)))


getPreds = SStateM (\(f,r,vs) -> return (Right ((f,r,vs),preds r)))
  where preds (Sig (Await x _)) = x

getPred n = getPreds >>= return . lookup n

give n = sget >>= (return . lookup n)


sput :: Monad m => HList ValOf ns -> SStateM ns e r m ()
sput a = SStateM (\(f,r,_) -> return (Right ((f,r,a),())))

runStateS
  :: (e -> m ())
     -> HList ValOf ns
     -> Sig ns e r
     -> SStateM ns e r m p
     -> m (Either r (SState m ns e r, p))
runStateS p s r (SStateM f) = f (p,r,s)


all n = waitFor (exper (n :? Any)) >>= neq
  where neq v = emit v >> waitFor (exper (n :? Neq v)) >>= neq

change n f = do v1 <- exper (n :? Any)
                v2 <- exper (n :? Neq v1)     
                return (f v1 v2)
