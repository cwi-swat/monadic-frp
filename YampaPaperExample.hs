{-# LANGUAGE TupleSections, ViewPatterns, Arrows #-}

module Main where


import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.Loops
import Data.IORef
import FRP.Yampa              as Yampa hiding (normalize)
import FRP.Yampa.Geometry hiding (normalize)
import Graphics.UI.SDL        as SDL
import Debug.Trace
import qualified Graphics.UI.SDL.Events as SDL.Events
import qualified Graphics.UI.SDL.Keysym as SDL.Keysym


type Point = (Double,Double)
data MouseBtn = MLeft | MMiddle | MRight deriving (Ord,Eq,Show)


type MouseDown = Yampa.Event (Set.Set MouseBtn)
type MouseUp   = Yampa.Event (Set.Set MouseBtn)

type MousePos = Point

type SDLIn = (MouseDown,MouseUp, MousePos)

clickOn :: MouseBtn -> SF (MouseDown) (Yampa.Event ())
clickOn b = arr $ fmap (const ()) . filterE (b `Set.member`)

sameClick :: SF MouseDown (Yampa.Event ())
sameClick = accumFilter isSame Nothing where
  isSame (Just s) s2 | s == s2 = (Nothing,Just ())
  isSame _        s2           = (Just s2, Nothing)

leftClick = clickOn MLeft
middleClick = clickOn MMiddle
rightClick = clickOn MRight

releaseOf :: MouseBtn -> SF MouseUp (Yampa.Event ())
releaseOf = clickOn

leftUp = releaseOf MLeft

before :: SF (Yampa.Event a, Yampa.Event b) (Yampa.Event Bool)
before = arr decide where
  decide ( _, Yampa.Event _) = Yampa.Event False
  decide (Yampa.Event _, _)       = Yampa.Event True
  decide _             = Yampa.NoEvent

addNeverL :: SF a b -> SF a (Yampa.Event c, b)
addNeverL x = x >>> arr (Yampa.NoEvent,)


doubler = constant Yampa.NoEvent where
 bla = 
  dSwitch (addNeverL (rightClick)) 
  (\_ -> dSwitch (proc d -> do 
             r <- after 0.2 () -< () 
             l <- rightClick -< d
             b <- before -< (l,r)
             returnA -< (Yampa.NoEvent,b)) 
          (\x -> if x then switch (arr (\_ -> (Event (),Event ()))) (\_ -> doubler) else doubler)
         
  )

data RRect = RRect Point Point deriving Show
data CColor = CColor { r :: Double, g :: Double, b :: Double } deriving Show
data Box = Box RRect CColor deriving Show

cycleColor :: SF SDLIn (CColor, Yampa.Event Int)
cycleColor = cc colors 0 where
  cc (h : t) i = dSwitch (proc (md,mu,mp) -> do
                mc <- middleClick -< md
                rc <- rightClick -< md
                returnA -< ((h,fmap (const i) rc),fmap (const i) mc) ) 
                (\i -> cc t (i+1))
           
wiggleRect :: RRect -> SF a RRect
wiggleRect (RRect lu rd) = localTime >>> arr rectAtTime
   where rectAtTime t =  RRect (lu +. dx) (rd +. dx)
                        where dx = (sin (t * 5) * 15, 0)

(x,y) +. (x',y') = (x + x', y + y')

inside :: Point -> RRect -> Bool
(x,y) `inside` (RRect (lx,uy) (rx,dy)) =
  x >= lx && x <= rx && y >= uy && y <= dy



colors = cycle [CColor 1 0 0 , CColor 0 1 0 , CColor 0 0 1 , CColor 1 1 0, CColor 0 1 1 , CColor 1 0 1, CColor 0 1 1]

chooseBoxColor :: RRect -> SF SDLIn (Box, Yampa.Event ())
chooseBoxColor rect =
  proc s -> do
    r' <- wiggleRect rect -< ()
    (c,e)  <- cycleColor -< s 
    rect <- arr (uncurry Box) -< (r',c) 
    returnA -< (rect,fmap (const ()) e)   

box :: Point -> SF SDLIn Box
box p1 =     dSwitch (curRect p1 >>> first (arr (setColor)))
             (\r -> dSwitch (chooseBoxColor r)
             constant)
  where setColor r = Box r (head colors)
        



curRect :: Point -> SF SDLIn (RRect, Yampa.Event RRect)
curRect p1 = proc (md,mu,mp) -> do  
               r <- arr (RRect p1) -< mp
               end <- leftUp -< mu
               returnA -< (r,fmap (const r) end)

boxes :: SF (MouseDown, MouseUp,MousePos) [Box]
boxes = boxes' [] where
  boxes' i = dpSwitch (\i l -> map (i,) l) i
                 (proc ((md,mu,mp),l) -> do
                    lc <- leftClick -< md
                    rc <- rightClick  -< md
                    dels <- arr findDels -< (rc,mp,l)
                    arr ans -< (dels,lc,mp))
                 mutateList 
  ans ([],Yampa.NoEvent,_) = Yampa.NoEvent
  ans (l, Yampa.Event _ , p) = Yampa.Event (l, Just p)
  ans (l, Yampa.NoEvent, _) = Yampa.Event (l,Nothing)
  mutateList l (lmin,d) =  newHead d ++ dels l lmin


newHead (Just p) = [box p]
newHead Nothing = []
  

dels l lmin=  map fst $ filter (not . snd) $ zip l lmin
findDels (Yampa.NoEvent,_,l) =  map (\b -> False) l
findDels (_, p, l) =  map (\(Box r c) -> ( p `inside` r)) l

main = interpretBoxes (800,600) boxes

----- interface SDL

normalize (RRect (lx,uy) (rx,dy)) = RRect (min lx rx, min uy dy) (max lx rx, max uy dy)

toRect :: RRect -> SDL.Rect
toRect (normalize -> RRect (lx,uy) (rx,dy)) = SDL.Rect (round lx) (round uy) (round (rx - lx)) (round (dy - uy))

getColor :: Surface -> CColor -> IO Pixel
getColor s c = 
     let fmt = surfaceGetPixelFormat s in
     mapRGB fmt (con r) (con g) (con b)
  where con d = round ( (d c) * 255.0)



draw :: Surface -> [Box] -> IO ()
draw s [] = return ()
draw s ((Box r c):t) =
  do p <- getColor s c
     fillRect s (Just $ toRect r) p
     draw s t
     
drawBoxes s l = 
  do  p <- getColor s (CColor 0 0 0)
      print (show l)
      fillRect s (Just $ SDL.Rect 0 0 1200 1000) p
      draw s (reverse l)
      SDL.flip s
      return False

-- | Interpet the given signal computation, drawing the boxes on screen as they are emmitted. The first argument is the width and height of the screen.

interpretBoxes :: Point -> SF (MouseDown, MouseUp, MousePos) [Box] -> IO ()
interpretBoxes (x,y) r = 
  do SDL.init [InitEverything]
     setVideoMode (floor x) (floor y) 32 [DoubleBuf]
     screen <- getVideoSurface
     t <- curTime
     tref <- newIORef t
     mpref <- newIORef (0,0)
     let handleReqs' = handleReqs tref mpref
     let drawBoxes' r = drawBoxes screen
     print "starting"
     reactimate (return (Yampa.NoEvent,Yampa.NoEvent, (0,0))) handleReqs' drawBoxes' r 

handleReqs :: IORef Double -> IORef Point -> Bool -> IO (DTime, Maybe (MouseDown , MouseUp, Point))
handleReqs tref mpref _= 
  do  t <- readIORef tref 
      t2 <- curTime
      mp <- readIORef mpref
      let tdiff = t2 - t
      mp <- readIORef mpref
      s <- getSDLOccs mp
      let (mp2,md,mu) = s
      writeIORef mpref mp2
      writeIORef tref t2
      let mds = if Set.null md then Yampa.NoEvent else Yampa.Event md
      let mus = if Set.null mu then Yampa.NoEvent else Yampa.Event mu
      return (tdiff, Just (mds,mus,mp2))

getSDLOccs :: Point -> IO (Point, Set.Set MouseBtn, Set.Set MouseBtn)
getSDLOccs mp = do  l <- getSDLEvs
                    let state = foldl merge (mp, Set.empty, Set.empty) l
                    return state
  where merge (a,b,c) (MouseMotion x y _ _)    = ((fromIntegral x,fromIntegral y) , b,c)
        merge (a,b,c) (MouseButtonDown _ _ x)  = (a,b `Set.union` (toM x), c)
        merge (a,b,c) (MouseButtonUp _ _ x)    = (a,b, c `Set.union` (toM x))
        merge (a,b,c) _                        = (a,b,c)


toM ButtonLeft    = Set.singleton MLeft
toM ButtonMiddle  = Set.singleton MMiddle
toM ButtonRight   = Set.singleton MRight
toM _             = Set.empty

getSDLEvs = 
  do h <- pollEvent
     case h of
       SDL.NoEvent -> return []
       Quit -> error "Quit!"
       _ -> do  t <- getSDLEvs
                return (h:t)
              
filterMap f = catMaybes . map f
inf = 1.0/0.0

curTime :: IO Double
curTime = do t <- getTicks
             return ((fromIntegral t) / 1000.0)

