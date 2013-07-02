{-# LANGUAGE DoAndIfThenElse #-}

-- | Tabbed boxes variant of the paper example.
-- Tabs are controlled by mouse gestures: a mouse gesture is holding down the right button and moving left, right, up or down and then releasing the button
--
-- * Mouse gesture up is duplicate the current tab
--
-- * Mouse gesture down is delete current tab
--
-- * Mouse gesture left is switch to tab on the right (this makes sense when making the movement)
--
-- * Mouse gesture right is switch to tab on the left (this makes sense when making the movement)
--
-- Tab actions are super awesomely animated! 
module TabbedBoxes where

import PaperExample
import Control.SFRP hiding (update)
import SDLSFRP
import Prelude hiding (null,map,filter,filter,until,repeat,cycle,scanl,span,break,either,foldl,length,splitAt,drop,take)
import qualified Prelude as P
import Data.Maybe
import Data.Sequence



data Gestures = GUp | GRight | GDown | GLeft deriving Show

gestureType (x,y) 
  | y < 0 && -y > abs x  = GUp
  | x > 0 && x > abs y   = GRight
  | y > 0 && y > abs x   = GDown
  | otherwise            = GLeft

ensure p = do a <- p
              case a of
                Just x -> return x
                Nothing -> ensure p

-- Wait for a mouse gesture: 
-- a mouse gesture is holding down the right button and moving left, right, up or down and then releasing the button
gesture = do  p1 <- ensure (mousePos `at` rightClick)
              r <- before (sleep 0.2) rightUp
              if not r 
              then gesture 
              else do  mp2 <- mousePos `at` rightUp
                       case mp2 of
                        Just p2 -> return (gestureType (p2 -. p1))
                        Nothing -> gesture



tabs :: Int -> Seq (ISigg [Box] ()) -> Sigg [Box] ()
tabs i l =
     do  let il = index l i
         (il,Done g) <- emitAll (il `iuntil` gesture)
         let l' = update i il l
         let li = getHead i l'
         case g of
           -- duplicate tabs
           GUp 
             -> do animateDup li
                   let l'' = insert (i + 1) il l'
                   tabs (i + 1) l''
           -- delete current tab
           GDown  | length l' > 1 
              -> do if i /= length l' - 1 
                    then do let ri = getHead (i + 1) l'
                            animateDeleleteToR li ri
                            let l'' = delete i l'
                            tabs i l''
                    else do let ri = getHead (i - 1) l' 
                            animateDeleleteToL ri li
                            let l'' = delete i l'
                            tabs (i-1) l''
           -- switch right
           GLeft  | i + 1 < length l' 
              -> do let ri = getHead (i + 1) l'
                    animateSwitchRight li ri
                    tabs (i + 1) l'
           -- switch left
           GRight | i > 0 
              ->  do let ri = getHead (i - 1) l'
                     animateSwitchLeft li ri
                     tabs (i - 1) l'
           _  -> tabs i l'
  where animateDup         l   = pure (++) <*> pure l <*> animateExitLeft l
        animateDeleleteToR l r = pure (++) <*> animateExitDown l <*> animateEnterRight r 
        animateDeleleteToL r l = pure (++) <*> animateExitDown r <*> animateEnterLeft l
        animateSwitchRight l r = pure (++) <*> animateExitLeft l <*> animateEnterRight r
        animateSwitchLeft  l r = pure (++) <*> animateExitRight l <*> animateEnterLeft r

animPoints = map (\t -> sin (t * 0.5 * pi)) (frac 0.7)
animateMove f = map f animPoints

animateEnterLeft   b = animateMove (\t -> moveBoxes ((-screenWidth,0) *. (1 -t)) b)
animateEnterRight  b = animateMove (\t -> moveBoxes ((screenWidth,0) *. (1 -t)) b)
animateExitLeft    b = animateMove (\t -> moveBoxes ((-screenWidth,0) *. t) b)
animateExitRight   b = animateMove (\t -> moveBoxes ((screenWidth,0) *. t) b)
animateExitDown    b = animateMove (\t -> moveBoxes ((0,-screenHeight) *. t) b)


tabbedBoxes = tabs 0 (singleton (iparList (spawn box)))

screenWidth  = 1200
screenHeight = 1000
screenSize   = (screenWidth,screenHeight)

delete i l = let (a,b) = splitAt i l in a >< (drop 1 b)
insert i x l = let (a,b) = splitAt i l in a >< (x <| b)

getHead i l = let h :| _ = index l i in h

frac t = map (/t) elapsed `until` (sleep t) >> return ()
a -. (x,y) = a +. (-x,-y)

(x,y) *. b = (x * b, y * b)

moveRect d (Rect p1 p2) = Rect (p1 +. d) (p2 +. d)
moveBox d (Box r c) = Box (moveRect d r ) c
moveBoxes d b  = P.map (moveBox d) b


