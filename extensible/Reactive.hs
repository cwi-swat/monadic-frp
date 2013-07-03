{-# LANGUAGE UndecidableInstances,TupleSections, ViewPatterns, TypeFamilies,NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses,GADTs, TypeOperators,FlexibleContexts  #-}

module Reactive where

import HMap
import Data.Maybe
import Data.Set hiding (map,filter,foldl)
import qualified Data.Set as Set
import Prelude hiding (lookup,null,map,filter,filter,until,repeat,cycle,scanl,span,break,either,foldl,mod,all)
import Control.Monad.Trans


data React v p a 
  = Done a
  | Await p (v -> React v p a)


instance Monad (React v p) where
  return              = Done
  (Await e c)  >>= f  = Await e ((>>= f) . c)
  (Done v)     >>= f  = f v


first
  :: (Satisfies v p, PredList p) =>
     React v p a -> React v p b -> React v p (React v p a, React v p b)
first l r = case (l,r) of
  (Await el _, Await er _) -> 
      let  e    = el `hunion` er
           c b  = first (update l b) (update r b)
      in Await e c
  _ -> Done (l,r)

update :: Satisfies v p => React v p a -> v -> React v p a
update (Await e c) b  | satisfies b e = c b
update r           _  = r

exper
  :: (Has n a v, HasPred n a p, PredList p) =>
     n -> Predicate a -> React v p a
exper n p = Await ps (\s -> Done (hlookup n s))
  where ps = addPred n p noPreds


interpret :: Monad m =>
     (p -> m v) -> 
     React v p a -> m a
interpret p (Done a)     = return a
interpret p (Await e c)  = p e >>= interpret p . c

-- | A signal computation is a reactive computation of an initialized signal
newtype  Sig     p v a b     =  Sig (React p v (ISig p v a b)) 
-- | An initialized signal 
data     ISig    p v a b     =  a :| (Sig p v a b) 
                             |  End b

interpretSig
  :: Monad m =>
     (p -> m v)
     -> (e -> m ()) -> Sig v p e r -> m r
interpretSig p d (Sig s) = 
  do  l <- interpret p s
      case l of
        h :| t  ->  d h >> interpretSig p d t
        End a   -> return a

instance Monad (Sig p v a) where
  return a = emitAll (End a)
  (Sig l) >>= f = Sig (l >>= ib)
   where  ib (h :| t)  = return (h :| (t >>= f))
          ib (End a)   = let Sig x = f a in x

instance Monad (ISig p v a) where
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
foldl :: (a -> b -> a) -> a -> Sig v p b r -> React v p a
foldl   f i (Sig l)   = l >>= ifoldl f i
ifoldl :: (a -> b -> a) -> a -> ISig v p b r -> React v p a
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

never = Await noPreds undefined

-- | Convert the result of a signal computation to a reactive computation.
res (Sig l)    = l >>= ires 
-- | Convert the result of an initialized signal a reactive computation.
ires (_ :| t)  = res t; ires (End a)   = Done a

instance Functor (React p v) where
  fmap f a = a >>= return . f
-- | Return the result of a reactive computation if it is done
done (Done a)              =  Just a  ; done _  = Nothing
-- | Give the current value of a signal computation, if any.
cur (Sig (Done (h :| _)))  = Just h  ; cur _   = Nothing

-- | the head of an initalized signal, if any.
icur (h :| t) = Just h
icur (End _)  = Nothing 

-- | Version of 'done' that throws an error if it the result is not done.
done' = fromJust . done


-- * Dynamic lists

-- | Cons the values from the first signal computation to the values form the latter signal computation over time.

cons h t = do  (h,t) <- imap (uncurry (:)) (pairs h t)
               imap (: []) h
               t
               return ()
-- | Run the initialized signals from the given signal computation in parallel, and emit the lists of the current form of all alive initialized signals.
parList x = emitAll (iparList x)



iparList l = rl ([] :| hold) l >> return () where
  rl t (Sig es) = do  (t,es) <- t `iuntil` es
                      case es of
                        Done (e :| es)  -> rl (cons e t) es
                        _               -> t 
                        

all n = waitFor (exper n Any) >>= change
   where change v = emit v >> waitFor (exper n (Neq v)) >>= change
          

change n f = do v1 <- exper n Any
                v2 <- exper n (Neq v1)
                return (f v1 v2)

