{-# LANGUAGE TupleSections, ViewPatterns #-}
-- | SDL boxes interpreter for Symmetric FRP
module SDLSFRP(Point, MouseBtn(..), Time,GUIEv(..), Rect(..), Color(..), Box(..), normalize, interpretBoxes) where

import Prelude
import Graphics.UI.SDL hiding (update,Rect,Color,Event)
import Data.Set hiding (foldl, map, null)
import qualified Graphics.UI.SDL as SDL
import Control.MonadicFRP hiding (map,foldl)

import Data.Maybe
import qualified Data.Set as Set
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans

type Point     = (Double,Double) -- in pixels
data MouseBtn  = MLeft | MMiddle | MRight deriving (Ord,Eq,Show)
type Time      = Double -- in seconds


data GUIEv 
  =  MouseDown     (Event (Set MouseBtn))
  |  MouseUp       (Event (Set MouseBtn))
  |  MouseMove     (Event Point)
  |  DeltaTime     (Event Time)
  |  TryWait Time  (Event Time) 
            deriving (Eq,Show,Ord)


data Rect    = Rect {leftup :: Point, rightdown :: Point} deriving Show
data Color   = Color {  r :: Double, g :: Double, b :: Double} deriving Show
data Box     = Box Rect Color deriving Show


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

-- | Interpet the given signal computation, drawing the boxes on screen as they are emmitted. The first argument is the width and height of the screen.

interpretBoxes (x,y) r = 
  do SDL.init [InitEverything]
     setVideoMode (floor x) (floor y) 32 [DoubleBuf]
     screen <- getVideoSurface
     t <- curTime
     let drawBoxes' r = lift (drawBoxes screen r)
     evalStateT (interpretSig handleReqs drawBoxes' r) t


handleReqs r = 
  do  t <- get 
      t2 <- lift curTime
      let tdiff = t2 - t
      if tdiff >= time
       then do  put (t + time)
                return (makeTimeObs r time)
       else do  occs <- lift getSDLOccs
                let occs' = onlyWait (occs `Set.union` makeTimeObs r tdiff) `Set.intersection` r 
                if Set.null occs'
                 then handleReqs r -- unfortunatly the Haskell SDL library doesn't export SDL_WaitEventTimeout method
                                   -- so here we just keep polling. Note that if the library did export this method
                                   -- then this process would be slightly more efficient
                 else do put t2
                         return occs'
  where time = getWait r

onlyWait r | all isWait (Set.elems r) = Set.empty
  where isWait (TryWait _ _)  = True
        isWait _              = False
onlyWait r | otherwise = r
  

getSDLOccs = do  l <- getSDLEvs
                 let state = foldl merge (Nothing, Set.empty, Set.empty) l
                 return (makeOccs state)
  where merge (a,b,c) (MouseMotion x y _ _)    = (Just  (fromIntegral x,fromIntegral y) , b,c)
        merge (a,b,c) (MouseButtonDown _ _ x)  = (a,b `Set.union` (toM x), c)
        merge (a,b,c) (MouseButtonUp _ _ x)    = (a,b, c `Set.union` (toM x))
        merge (a,b,c) _                        = (a,b,c)
        mergeJust _ (Just p)  = Just p
        mergeJust (Just p) _  = Just p
        mergeJust _ _         = Nothing

toM ButtonLeft    = Set.singleton MLeft
toM ButtonMiddle  = Set.singleton MMiddle
toM ButtonRight   = Set.singleton MRight
toM _             = Set.empty


makeOccs (a,b,c) = mouseEv a `Set.union` butDownEv b `Set.union` butUpEv c
  where mouseEv (Just p) = Set.singleton (MouseMove (Occurred p))
        mouseEv Nothing  = Set.empty
        butDownEv s | Set.null s = Set.empty
                    | otherwise = Set.singleton (MouseDown (Occurred s))
        butUpEv s   | Set.null s = Set.empty
                    | otherwise = Set.singleton (MouseUp (Occurred s))
getSDLEvs = 
  do h <- pollEvent
     case h of
       NoEvent -> return []
       KeyDown k -> error "Quit"
       Quit -> error "Quit!"
       _ -> do  t <- getSDLEvs
                return (h:t)
              
filterMap f = catMaybes . map f

inf = 1.0/0.0

curTime :: IO Double
curTime = do t <- getTicks
             return ((fromIntegral t) / 1000.0)

getWait r = if null all then inf else minimum all
  where  all = filterMap getTime (Set.elems r)
         getTime (TryWait t _)     = Just t
         getTime _                 = Nothing

makeTimeObs r t = Set.fromList $ filterMap makeOcc (Set.elems r)
  where  makeOcc (TryWait t' _)     = Just $ TryWait t' (Occurred t)
         makeOcc (DeltaTime _)      = Just $ DeltaTime (Occurred t)
         makeOcc _                  = Nothing
