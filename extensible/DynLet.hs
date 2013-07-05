{-# LANGUAGE UndecidableInstances, ScopedTypeVariables,NoMonomorphismRestriction,OverlappingInstances,FlexibleInstances,FunctionalDependencies,MultiParamTypeClasses, TypeOperators,FlexibleContexts  #-}

module DynLet where

import HEnv
import Reactive

upSelf :: forall n v l. (Var n v, Has n v l, MMapOps l) => l V ->  ISige l v-> (ISige l v ,l V)
upSelf a (h :- Sig t) = 
  case done t' of
    Just (h' :| t') -> let a' :: l V = hmod (getVar :: n) (onV (const h') :: V v -> V v) a 
                       in upSelf a' (h' :- t')
    Nothing -> (h :- Sig t',a)
  where t' = updateOnce t a

data ISige l e = e :- Sig l e ()

extendWith :: (MMapOps l, MMapConcat l v r) => l (ISige v) -> v V -> r V
extendWith l v = hconcat (hmap toV l) v
  where toV (h :- t) = V h


fixUp :: (MMapOps l, MMapOps r) =>  r V -> l (ISige r) -> (l (ISige r), r V)
fixUp v ss = hfoldbuild upSelf v ss

{-
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

        
