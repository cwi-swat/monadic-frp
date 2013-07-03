{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses, TypeSynonymInstances, TypeOperators,FlexibleContexts, TypeFamilies, TupleSections, ViewPatterns #-}
-- | SDL boxes interpreter for Symmetric FRP
module SDLSFRP where

import Prelude
import Graphics.UI.SDL hiding (update,Rect,Color,Event)
import Reactive hiding (foldl,map)
import HMap 
import Data.Set hiding (foldl, map, null)
import qualified Data.Set as Set
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
instance Var MouseBtns (Set MouseBtn)
instance Var MousePos Point
instance Var Dt Time


type SDLVals = MouseBtns := Set MouseBtn :&  MousePos := Point :& Dt := Time :& X
type SDLPreds = MouseBtns := PredSet (Set MouseBtn) :&  MousePos := PredSet Point :& Dt := PredSet Time :& X

initS :: SDLVals
initS = V empty :& V (0,0) :& V 0 :& X 



interpretBoxes :: (Double,Double) -> Sig SDLVals SDLPreds [Box] a -> IO a
interpretBoxes (x,y) r = 
  do SDL.init [InitEverything]
     setVideoMode (floor x) (floor y) 32 [DoubleBuf]
     screen <- getVideoSurface
     t <- curTime
     evalStateT (loop screen r) (t,initS) 


loop s (Sig (done -> Just (h :| t)))  = lift (drawBoxes s h) >> loop s t
loop s (Sig (done -> Just (End a)))  = return a
loop s (Sig r@(Await p c)) = upState p >>= loop s . Sig . update r

upState :: SDLPreds -> StateT (Time,SDLVals) IO SDLVals
upState p = do 
             (prevt,s) <- get
             t <- lift curTime
             let dt = t - prevt
             let ps = getPred Dt p :: PredSet Double
             let mdt = maxTime ps :: Double
             if mdt < dt 
             then
              do let s' = hmod Dt (const mdt :: Double -> Double) s
                 put (prevt + mdt, s')
                 return s'
             else 
              do evs <- lift getSDLEvs
                 let s'  = foldl handleEv s evs
                 let s'' = hmod Dt (const dt) s'
                 put (t,s'')
                 return s''
             

maxTime ts | null ms = 1.0 / 0
           | otherwise =  maximum ms
  where ms = mapFilter fromEq (elems ts)
        fromEq (Leq n) = Just n
        fromEq _       = Nothing

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f = catMaybes . map f

getSDLEvs = pollEvent >>= 
            \h -> case h of
              NoEvent -> return []
              Quit -> error "Quit!"
              _ -> do  t <- getSDLEvs
                       return (h:t)

handleEv :: SDLVals -> SDL.Event -> SDLVals
handleEv l (MouseMotion x y _ _)    = hmod MousePos (const (fromIntegral x :: Double,fromIntegral y :: Double)) l
handleEv l (MouseButtonDown _ _ x)  = hmod MouseBtns (insert (toM x)) l
handleEv l (MouseButtonUp  _ _ x)   = hmod MouseBtns (delete (toM x)) l
handleEv l _                        = l

curTime = do t <-  getTicks
             return ((fromIntegral t) / 1000.0)

toM ButtonLeft    = MLeft
toM ButtonMiddle  = MMiddle
toM ButtonRight   = MRight
toM _             = MMiddle



