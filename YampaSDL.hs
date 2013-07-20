{-# LANGUAGE TupleSections, ViewPatterns, Arrows #-}


module YampaSDL(Point,MouseBtn(..),MouseDown,MouseUp,MousePos,Rect(..),Color(..),Box(..),SDLIn, interpretBoxes) where

import Graphics.UI.SDL    hiding (Rect,Color)
import qualified Graphics.UI.SDL as SDL 
import qualified Graphics.UI.SDL.Events as SDL.Events
import qualified Graphics.UI.SDL.Keysym as SDL.Keysym
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import FRP.Yampa              as Yampa hiding (normalize)
import FRP.Yampa.Geometry hiding (normalize)
import Debug.Trace

type Point = (Double,Double)
data MouseBtn = MLeft | MMiddle | MRight deriving (Ord,Eq,Show)


type MouseDown = Yampa.Event (Set.Set MouseBtn)
type MouseUp   = Yampa.Event (Set.Set MouseBtn)

type MousePos = Point

data Rect = Rect Point Point deriving Show
data Color = Color { r :: Double, g :: Double, b :: Double } deriving Show
data Box = Box Rect Color deriving Show

type SDLIn = (MouseDown,MouseUp, MousePos)

----- interface SDL

normalize (Rect (lx,uy) (rx,dy)) = Rect (min lx rx, min uy dy) (max lx rx, max uy dy)

toRect :: Rect -> SDL.Rect
toRect (normalize -> Rect (lx,uy) (rx,dy)) = SDL.Rect (round lx) (round uy) (round (rx - lx)) (round (dy - uy))

getColor :: Surface -> Color -> IO Pixel
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
  do  p <- getColor s (Color 0 0 0)
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
        merge (a,b,c) (MouseButtonDown _ _ x)  = (a,trace "bla" $ b `Set.union` (toM x), c)
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
