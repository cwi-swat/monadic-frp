{-# LANGUAGE TupleSections, ViewPatterns, Arrows #-}

module Main where

import YampaSDL
import FRP.Yampa.Utilities
import qualified Data.Set as Set
import Data.Maybe

import FRP.Yampa              as Yampa hiding (normalize)
import FRP.Yampa.Geometry hiding (normalize)
import YampaSDL
import Debug.Trace




clickOn :: MouseBtn -> SF MouseDown (Event ())
clickOn b = arr $ \e -> fmap (const ()) $ filterE (\x -> b `Set.member` x) e 

sameClick :: SF MouseDown (Event ())
sameClick = accumFilter isSame Nothing where
  isSame (Just s) s2 | s == s2 = (Nothing,Just ())
  isSame _        s2           = (Just s2, Nothing)

leftClick = clickOn MLeft
middleClick = clickOn MMiddle
rightClick = clickOn MRight

releaseOf :: MouseBtn -> SF MouseUp (Event ())
releaseOf = clickOn

leftUp = releaseOf MLeft

before :: SF (Event a, Event b) (Event Bool)
before = arr decide where
  decide ( _, Event _) = Event False
  decide (Event _, _)       = Event True
  decide _             = Yampa.NoEvent

addNeverL :: SF a b -> SF a (Event c, b)
addNeverL x = x >>> arr (NoEvent,)


green = head colors
red = green
orange = green



traffic2 = pure green `switcher` setNext phOrange rightClick
  phOrang = pure orange `switcher` merge (setNext phRed rightClick) (setNext traffic2 leftClick)
  phRed   = pure red
  setNext m = fmap (const m)

traffic :: SF MouseDown Color
traffic = switch (constant green &&& notYet <<< rightClick) $ \_ ->  
          switch (constant orange &&& leftClick `tagBool` rightClick)
          $ \r ->  if r
                   then constant red
                   else traffic
  where tagBool :: SF a (Event x) -> SF a (Event y) -> SF a (Event Bool)
        tagBool x y = arr2 lMerge <<< tag' True x &&& tag' False y
        tag' t a = arr (`tag` t) <<< a

doublerr = constant NoEvent where
 bla = 
  dSwitch (addNeverL (rightClick)) 
  (\_ -> dSwitch (proc d -> do 
             r <- after 0.2 () -< () 
             l <- rightClick -< d
             b <- before -< (l,r)
             returnA -< (Yampa.NoEvent,b)) 
          (\x -> if x then switch (arr (\_ -> (Event (),Event ()))) (\_ -> doublerr) else doublerr)
         
  )


--type MouseDown = Event (Set.Set MouseBtn)
--type MouseUp = Event (Set.Set MouseBtn)

countSig :: SF (Event a) Int
countSig = hold 0 <<< count

advList :: [a] -> SF (Event x) a
advList (h : t) =  
  identity &&& arr (\x -> tag x (advList t)) 
  >>> rSwitch (constant h)

cycleColor2 :: SF MouseDown (Color, Event Int)
cycleColor2 = proc md -> do
   mc <- middleClick     -< md 
   rc <- rightClick      -< md   
   c  <- advList colors  -< mc
   n  <- countSig        -< mc
   e  <- arr2 tag        -< (rc,n)
   returnA -< (c,e) 
     
{-
ding :: SF (Event Int) Int
ding = damin 0 0 where
  damin  mi mx  = switch (add mi)         (\i -> damax  (min mi i) (max mx i))
  damax  mi mx  = switch (add mx)         (\i -> daplus (min mi i) (max mx i))
  daplus mi mx  = switch (add $ mi + mx)  (\i -> damin  (min mi i) (max mx i)) 
  where add h = constant h &&& identity
-}

   
   

cycleColor :: SF MouseDown (Color, Event Int)
cycleColor = cc colors 0 where
  cc (h : t) i = 
    switch ( proc md -> do
         mc  <- notYet <<< middleClick  -< md
         rc  <- rightClick              -< md
         returnA -< ((h,tag rc i), mc)
    ) (\_ -> cc t (i+1))
           
wiggleRect :: Rect -> SF a Rect
wiggleRect (Rect lu rd) = localTime >>> arr rectAtTime
   where rectAtTime t =  Rect (lu +. dx) (rd +. dx)
                        where dx = (sin (t * 5) * 15, 0)

(x,y) +. (x',y') = (x + x', y + y')

inside :: Point -> Rect -> Bool
(x,y) `inside` (Rect (lx,uy) (rx,dy)) =
  x >= lx && x <= rx && y >= uy && y <= dy



colors = cycle [Color 1 0 0 , Color 0 1 0 , Color 0 0 1 , Color 1 1 0, Color 0 1 1 , Color 1 0 1, Color 0 1 1]

chooseBoxColor :: Rect -> SF SDLIn (Box, Event Box)
chooseBoxColor rect =
  proc (md,mu,_) -> do
    r' <- wiggleRect rect -< ()
    (c,e)  <- cycleColor -< md
    box <- arr (uncurry Box) -< (r',c) 
    returnA -< (box,fmap (const box) e)   

box :: Point -> SF SDLIn (Box,Event ())
box p1 =     dSwitch (proc s -> do
                  (r,e) <- curRect p1 -< s
                  returnA -< ((setColor r, NoEvent),e))
             (\r -> dSwitch (first (arr (,NoEvent)) <<< chooseBoxColor r)
             (\b -> proc (md,mu,mp) -> do
                      dr <- rightClick -< md
                      inn <- arr (`inside` r) -< mp
                      returnA -< (b, fmap (const ()) $ gate dr inn )
             ))
  where setColor r = Box r (head colors)
        



curRect :: Point -> SF SDLIn (Rect, Event Rect)
curRect p1 = proc (md,mu,mp) -> do  
               r <- arr (Rect p1) -< mp
               end <- leftUp -< mu
               returnA -< (r,fmap (const r) end)

newBox = proc (md,_,mp) -> do
  lc <- leftClick -< md
  r <- arr (fmap box) -< fmap (const mp) lc
  returnA -< r

type In = (MouseDown, MouseUp,MousePos) 

boxes :: SF In [Box]
boxes = boxes' [] >>> arr (map fst) where
  boxes' i = pSwitchList i
    (newBox *** arr toEv >>> arr choose >>> notYet)
    (\e l -> boxes' (mutateList e l))
  choose (a,b) = merge (fmap Left a) (fmap Right b)
  toEv l = let l' = map (isNoEvent . snd) l in if and l' then NoEvent else Event l'
  mutateList :: [SF In (Box,Event ())] -> Either (SF In (Box,Event ())) [Bool] -> [SF In (Box,Event ())]
  mutateList l (Left b)   = b : l
  mutateList l (Right l') = map fst $ filter snd $ zip l l'
 


main = interpretBoxes (800,600) boxes

pSwitchList ::  [SF a b]  
            ->  SF (a, [b]) (Event c)
            ->  ([SF a b] -> c -> SF a [b])
            ->  SF a [b]
pSwitchList = dpSwitch (\i l -> map (i,) l)

