{-# LANGUAGE KindSignatures,ViewPatterns,UndecidableInstances, ScopedTypeVariables,NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses, TypeOperators,FlexibleContexts  #-}

module DynLet where

import HEnv
import ReactiveHEnv
import Reactive
import Debug.Trace
import qualified Data.Set as Set

infixr 7 :-

data ISige l e = e :- Sig l e ()

getPredI (_ :- Sig (Await e _)) = e
getPredI (_ :- Sig _) = build (PredSet Set.empty)

upSelf :: (Var n v, Has n v l, Ord v, UnionPreds l, Satisfies l)=> l V -> n -> ISige l v -> (ISige l v ,l V)
upSelf a n (h :- Sig t) = 
  case done t' of
    Just (h' :| t'') -> let a' = hmod n (onV (const h')) a
                        in  upSelf a' n (h' :- t'')
    Nothing -> (h :- Sig t',a)
  where t' = updateOnce t a

class GetVals (l :: (* -> *) -> *) where
  getVals :: l (ISige r) -> l V
  getPreds :: (UnionPreds r, Build r) => r PredSet -> l (ISige r) -> r PredSet

instance GetVals X where
  getVals l = X
  getPreds r X = r

instance (Var n v, GetVals t) => GetVals ((n :-> v) :& t) where
  getVals (h :& t) = vmod sel h :& getVals t
      where sel (e :- _) = V e
  getPreds r (n :-> (getPredI -> v) :& t) = getPreds (unionPreds r v) t

class HEq l where
  heq :: l V -> l V -> Bool

instance HEq X where
  heq X X = True

instance (Var n v, Eq v, HEq t) => HEq ((n :-> v) :& t) where
  heq ((vget -> V a) :& tl) ((vget -> V b) :& tr) = a == b && heq tl tr

extendWith :: (GetVals l, MMapConcat l v r) => l (ISige r) -> v V -> r V
extendWith l v = hconcat (getVals l) v

class FixUp l r where
  fixUp :: l V -> r (ISige l) -> (l V, r (ISige l))

instance FixUp l X where
  fixUp l X = (l,X)

instance (Var n v,  Ord v, Has n v l, UnionPreds l, Satisfies l, FixUp l t) => FixUp l ((n :-> v) :& t) where
   fixUp l (n :-> h :& t) = (l'', n :-> h' :& t')
        where (h' ,l') = upSelf l n h
              (l'' ,t') = fixUp l' t

fixpointUp ss l = if isEq then (ss',l') else fixpointUp ss' l'
  where (l', ss') = fixUp l ss
        isEq = l `heq` l'


dynlet ss (Sig r@(Await p c)) =   Sig (Await ps cont)
  where cont e = let Sig t =  dynlet ss' (Sig (update r e'')) in t
                 where e' =extendWith ss e
                       (ss',e'') = fixpointUp ss e'
        ps = hremove (getPreds p ss) ss
dynlet ss (Sig (Done (h :| t))) = Sig (Done (h :| dynlet ss t))
dynlet ss (Sig (Done (End a))) = Sig (Done (End a))

 


{--





dynlet :: forall vl vi vo pvo pvi e r. (AddVars vl vi vo, UpVars vl vo, HEq vo,
            Satisfies vo pvo, Satisfies vi pvi, RemoveVars pvo pvi) => 
            vl -> Sig vo pvo e r -> 
            Sig vi pvi e r
dynlet ss (Sig r@(Await p c)) = Sig (Await (removeVars p) cont )
  where cont e = r'
         where (e' :: vo, ss' :: vl) = fixVars e ss
               Sig r' = dynlet ss' (Sig $ update r e')


        
         
dynlet ss (Sig (Done (h :| t))) = Sig (Done $ h :| dynlet ss t)
dynlet ss (Sig (Done (End a))) = Sig (Done (End a))


outsidec :: forall vl vi vo pvo pvi e r. (AddVars vl vi vo, UpVars vl vo, HEq vo,
            Satisfies vo pvo, Satisfies vi pvi) => 
            React vo pvo (ISig vo pvo e r) -> 
            vl -> vi -> (React vo pvo (ISig vo pvo e r), vl)
outsidec r ss e = (update r e',ss') 
   where (e' :: vo,ss' :: vl) = fixVars e ss 

fixVars :: (AddVars vl vi vo, UpVars vl vo, HEq vo) => vi -> vl -> (vo,vl)
fixVars e ss = fixUp e' ss
  where e' = addVars ss e 
--}

        
