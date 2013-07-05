-- | The example from the paper
{-# LANGUAGE MultiParamTypeClasses,TupleSections, ViewPatterns, NoMonomorphismRestriction #-}
module PaperExample where

import Reactive
import Data.Set hiding (map,filter)
import Prelude hiding (lookup,null,map,filter,filter,until,repeat,cycle,scanl,span,break,either,foldl,mod,all)
import qualified Prelude as P
import Data.Maybe
import HEnv
import Boxes
import SDLSFRP
import qualified Data.Set as Set

data MouseOver  = MouseOver
instance Var MouseOver Bool

mouseBtns = all MouseBtns
mousePos = all MousePos
times = all Time

elapsed :: Sigg Seconds ()
elapsed  = do tbase <- waitFor (exper Time (Any :: Predicate Seconds))
              t2 <- waitFor (exper Time (Neq tbase))
              rest tbase t2
  where rest tbase tprev =
          do emit (tprev - tbase)
             t' <- waitFor (exper Time (Neq tprev))
             rest tbase t'
          
               


type Reactg = React SDLVars
type Sigg     = Sig    SDLVars 
type ISigg    = ISig  SDLVars

mouseDown :: Reactg (Set MouseBtn)
mouseDown = change MouseBtns (flip difference)

mouseUp :: Reactg (Set MouseBtn)
mouseUp = change MouseBtns difference



sameClick :: Reactg Bool
sameClick =  mouseDown >>= (\pressed   ->
             mouseDown >>= (\pressed2  ->
             return (pressed == pressed2)))


sameClick2 = do  pressed   <-  mouseDown
                 pressed2  <-  mouseDown
                 return (pressed == pressed2)

clickOn :: MouseBtn -> Reactg ()
clickOn b = do  bs <- mouseDown
                if (b `member` bs) then return () else clickOn b

leftClick    = clickOn MLeft
middleClick  = clickOn MMiddle
rightClick   = clickOn MRight

--implicit in paper
releaseOf :: MouseBtn -> Reactg ()
releaseOf b = do  bs <- mouseUp
                  if (b `member` bs) then return () else releaseOf b
leftUp    = releaseOf MLeft
middleUp  = releaseOf MMiddle
rightUp   = releaseOf MRight

before :: Reactg a -> Reactg b -> Reactg Bool
before a b = do  (a',b') <- first a b
                 case (done a', done b') of
                   (Just _, Nothing)  -> return True
                   _                  -> return False

doubler :: Reactg ()
doubler = do  rightClick
              r <- rightClick `before` sleep 0.2
              if r then return () else doubler



cycleColor :: Sigg Color Int
cycleColor = cc colors 0 where
  cc (h:t) i = 
    do  emit h
        r <- waitFor (before middleClick rightClick)
        if r then cc t (i+1) else return i



curRect :: Point -> Sigg Rect ()
curRect p1 = map (Rect p1) mousePos




wiggleRect :: Rect -> Sigg Rect ()
wiggleRect (Rect lu rd) = map rectAtTime elapsed
  where rectAtTime t =  Rect (lu +. dx) (rd +. dx)
                        where dx = (sin (t * 5) * 15, 0)

(x,y) +. (x',y') = (x + x', y + y')


posInside :: Rect -> Sigg Point y -> Reactg (Maybe Point)
posInside r = find (`inside` r)

inside :: Point -> Rect -> Bool
(x,y) `inside` (normalize -> Rect (lx,uy) (rx,dy)) = 
  x >= lx && x <= rx && y >= uy && y <= dy


firstPoint :: Reactg (Maybe Point) 
firstPoint = mousePos `at` leftClick


completeRect :: Point -> Sigg Rect (Maybe Rect)
completeRect p1 = do  (r,_) <- curRect p1 `until` leftUp
                      return (cur r)

defineRect :: Sigg Rect Rect
defineRect = do  Just p1  <-  waitFor firstPoint
                 Just r <- completeRect p1
                 return r


chooseBoxColor :: Rect -> Sigg Box ()
chooseBoxColor r = 
  do  pure Box <*> wiggleRect r <*> cycleColor
      return ()


drClickOn :: Rect -> Reactg (Maybe Point)
drClickOn r = 
  posInside r (mousePos `indexBy` repeat doubler)

box :: Sigg Box ()
box = do  r <- map setColor defineRect
          chooseBoxColor r
          waitFor (drClickOn r)
          return ()
  where setColor r = Box r (head colors)

{--
box2 :: Sigg Box ()
box2 = do r <- map setColor defineRect
          dynlet (
            MouseOver :== (False :| (map (`inside` r) mousePos ))
            :& X ) $ 
            do map (\x -> Box r x) $ map (\x -> if x then Color 1 0 0 else Color 1 1 1) $ all MouseOver
               
  where setColor r = Box r (head colors)
--}
boxes :: Sigg [Box] ()
boxes =  parList (spawn box)



sleep :: Seconds -> Reactg ()
sleep t = do  t1 <- exper Time Any
              exper Time (Eq (t1 + t))
              return ()



-- implicit in paper
colors = P.cycle [Color 1 0 0 , Color 0 1 0 , Color 0 0 1 , Color 1 1 0, Color 0 1 1 , Color 1 0 1, Color 0 1 1]


