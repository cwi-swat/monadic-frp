{-# LANGUAGE TypeFamilies, TupleSections, ViewPatterns #-}
-- | SDL boxes interpreter for Symmetric FRP
module SDLSFRP where

import Prelude
import Graphics.UI.SDL hiding (update,Rect,Color,Event)
import Reactive hiding (foldl,map)
import HMap 
import Data.Set hiding (foldl, map, null)
import qualified Graphics.UI.SDL as SDL
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Boxes


data MouseBtn  = MLeft | MMiddle | MRight deriving (Ord,Eq,Show)
type Time = Double

data MouseBtns  = MouseBtns
data MousePos   = MousePos
data Dt         = Dt
type instance TypeOf MouseBtns  = Set MouseBtn
type instance TypeOf MousePos   = Point
type instance TypeOf Dt         = Time

type SDLVars = Cons Dt (Cons MousePos (Cons MouseBtns Empty))



interpretBoxes :: (Double,Double) -> Sig SDLVars [Box] () -> IO ()
interpretBoxes (x,y) r = 
  do SDL.init [InitEverything]
     setVideoMode (floor x) (floor y) 32 [DoubleBuf]
     screen <- getVideoSurface
     t <- curTime
     let drawBoxes' r = lift (drawBoxes screen r)
     let init = ((Val 0) :& (Val (0,0) :& (Val empty :& X))) :: HList ValOf SDLVars
     let state = runStateS drawBoxes' init r upStates
     let run = evalStateT state t 
     run
     return ()
     

upStates = forever upState

upState :: SStateM SDLVars a r (StateT Time IO) ()
upState = do 
             prevt <- lift get
             l2 $ print (show prevt)
             t <- l2 curTime
             l2 $ print "Jalllooo"
             let dt = prevt - t
             
             ps <- getPred Dt
             let mdt = maxTime ps
             l2 $ print (show mdt)
             if mdt < dt 
             then
              do s <- sget
                 sput (modv Dt (const mdt) s)
                 lift (put (prevt + mdt))
             else 
              do evs <- l2 getSDLEvs
                 s <- sget
                 let s'  = foldl handleEv s evs
                 let s'' = modv Dt (const dt) s'
                 sput s''
                 lift (put t)
  where l2 = lift . lift
             

maxTime (PredsOf ts) = maximum $ mapFilter fromEq (elems ts)
  where fromEq (_ :? Leq n) = Just n
        fromEq _      = Nothing

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f = catMaybes . map f

getSDLEvs = pollEvent >>= 
            \h -> case h of
              NoEvent -> return []
              Quit -> error "Quit!"
              _ -> do  t <- getSDLEvs
                       return (h:t)

handleEv l (MouseMotion x y _ _)    = modv MousePos (const (fromIntegral x,fromIntegral y)) l
handleEv l (MouseButtonDown _ _ x)  = modv MouseBtns (insert (toM x)) l
handleEv l (MouseButtonUp  _ _ x)   = modv MouseBtns (delete (toM x)) l
handleEv l _                        = l

curTime = do t <-  getTicks
             return ((fromIntegral t) / 1000.0)

toM ButtonLeft    = MLeft
toM ButtonMiddle  = MMiddle
toM ButtonRight   = MRight
toM _             = MMiddle

