-- | The example from the paper
{-# LANGUAGE MultiParamTypeClasses,TupleSections, ViewPatterns, NoMonomorphismRestriction #-}
module PaperExample where

import Reactive
import ReactiveHEnv
import DynLet
import Data.Set hiding (map,filter)
import Prelude hiding (lookup,null,map,filter,filter,until,repeat,cycle,scanl,span,break,either,foldl,mod,all)
import qualified Prelude as P
import Data.Maybe
import HEnv
import Boxes
import SDLSFRP
import Debug.Trace
import qualified Data.Set as Set


mouseBtns = all MouseBtns
mousePos = all MousePos
times = all Time


elapsed  = do tbase <- waitFor (exper Time (Any :: Predicate Seconds))
              t2 <- waitFor (exper Time (Neq tbase))
              rest tbase t2
  where rest tbase tprev =
          do emit (tprev - tbase)
             t' <- waitFor (exper Time (Neq tprev))
             rest tbase t'
          
               
frac t = map (/ t) elapsed `until` sleep t



mouseDown = change MouseBtns (flip difference)

mouseUp = change MouseBtns difference



sameClick =  mouseDown >>= (\pressed   ->
             mouseDown >>= (\pressed2  ->
             return (pressed == pressed2)))


sameClick2 = do  pressed   <-  mouseDown
                 pressed2  <-  mouseDown
                 return (pressed == pressed2)

clickOn b = do  bs <- mouseDown
                if (b `member` bs) then return () else clickOn b

leftClick    = clickOn MLeft
middleClick  = clickOn MMiddle
rightClick   = clickOn MRight

--implicit in paper
releaseOf b = do  bs <- mouseUp
                  if (b `member` bs) then return () else releaseOf b
leftUp    = releaseOf MLeft
middleUp  = releaseOf MMiddle
rightUp   = releaseOf MRight

before a b = do  (a',b') <- first a b
                 case (done a', done b') of
                   (Just _, Nothing)  -> return True
                   _                  -> return False

doubler = do  rightClick
              r <- rightClick `before` sleep 0.2
              if r then return () else doubler



cycleColor = cc colors 0 where
  cc (h:t) i = 
    do  emit h
        r <- waitFor (before middleClick rightClick)
        if r then cc t (i+1) else return i




curRect p1 = map (Rect p1) mousePos





wiggleRect (Rect lu rd) = map rectAtTime elapsed
  where rectAtTime t =  Rect (lu +. dx) (rd +. dx)
                        where dx = (sin (t * 5) * 15, 0)

(x,y) +. (x',y') = (x + x', y + y')
(x,y) -. (x',y') = (x - x', y - y')

moveRect (Rect l r) x = Rect (l +. x) (r +. x)



posInside r = find (`inside` r)

inside :: Point -> Rect -> Bool
(x,y) `inside` (normalize -> Rect (lx,uy) (rx,dy)) = 
  x >= lx && x <= rx && y >= uy && y <= dy


firstPoint = mousePos `at` leftClick



completeRect p1 = do  (r,_) <- curRect p1 `until` leftUp
                      return (cur r)


defineRect = do  Just p1  <-  waitFor firstPoint
                 Just r <- completeRect p1
                 return r



chooseBoxColor r = 
  do  pure Box <*> wiggleRect r <*> cycleColor
      return ()



drClickOn r = 
  posInside r (map fst $ mousePos `indexBy` repeat doubler)


box = do  r <- map setColor defineRect
          chooseBoxColor r
          waitFor (drClickOn r)
          return ()
  where setColor r = Box r (head colors)


data MouseOver  = MouseOver 
instance Var MouseOver Bool
data VRect = VRect 
instance Var VRect Rect
data VColor = VColor
instance Var VColor Color

moves = waitFor (exper MousePos Any) >>= changes 
  where changes p =
           do p2 <- waitFor (exper MousePos (Neq p)) 
              emit (p2 -. p)
              changes p2
              
                 

dragRect r = 
  do waitFor (active $ repeat rightClick `during` (all MouseOver))
     p <- waitFor (exper MousePos Any)
     (cur -> Just r',_) <- scanl moveRect r moves `until` rightUp
     dragRect r'
     
brighten False b = b
brighten True b = lerpColor 0.5 b (Color 1 1 1)

destroyAnim (Box r c) = map (\t -> Box (shrink r t) c) $ frac 1 where
  shrink (Rect lu rd) t = Rect (lerpPoint lu center t) (lerpPoint rd center t) 
    where center = lerpPoint lu rd 0.5

box2 = do r <- map setColor defineRect
          (Just r) <- laste $ dynlet (
            MouseOver :-> False :- (pure inside <*> mousePos <*> all VRect  >> return () ) :&
            VColor  :-> Color 1 0 0 :- (pure brighten <*> all MouseOver <*> changes (repeat middleClick `during` all MouseOver) colors >> return ()) :&
            VRect :-> r :- dragRect r :& X )
            $ pure Box <*> all VRect <*> (all VColor `until` (active $ repeat doubler `during` (all MouseOver)))
          destroyAnim r
          return ()

  where setColor r = Box r (head colors)

changes :: Sig e a () -> [b] -> Sig e b ()
changes r l = map head $ scanl (\l _ -> tail l) l r


boxes =  parList (spawn box2)



sleep t = do  t1 <- exper Time Any
              exper Time (Eq (t1 + t))
              return ()



-- implicit in paper
colors = P.cycle [Color 1 0 0 , Color 0 1 0 , Color 0 0 1 , Color 1 1 0, Color 0 1 1 , Color 1 0 1, Color 0 1 1]


