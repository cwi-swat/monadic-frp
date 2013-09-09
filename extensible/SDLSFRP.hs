{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses, TypeSynonymInstances, TypeOperators,FlexibleContexts, TypeFamilies, TupleSections, ViewPatterns #-}
-- | SDL boxes interpreter for Symmetric FRP
module SDLSFRP where

import Prelude
import Graphics.UI.SDL hiding (update,Rect,Color,Event)
import Reactive hiding (foldl,map)
import HEnv 
import ReactiveHEnv
import Data.Set hiding (foldl, map, null)
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Boxes




data MouseBtn  = MLeft | MMiddle | MRight deriving (Ord,Eq,Show)
type Seconds = Double

data MouseBtns  = MouseBtns deriving Show
data MousePos   = MousePos deriving Show
data Time       = Time deriving Show
instance Var MouseBtns (Set MouseBtn) 
instance Var MousePos Point 
instance Var Time Seconds 


type SDLVars = (MouseBtns :-> Set MouseBtn :& MousePos :-> Point :& Time :-> Seconds :& X)

type SDLVals = SDLVars V
type SDLPreds = SDLVars PredSet

initS :: Seconds -> SDLVars V
initS s = MouseBtns :-> V empty :& MousePos :-> V (0,0) :& Time :-> V s :& X 



interpretBoxes :: (Double,Double) -> Sig SDLVars [Box] a -> IO a
interpretBoxes (x,y) r = 
  do SDL.init [InitEverything]
     setVideoMode (floor x) (floor y) 32 [DoubleBuf]
     screen <- getVideoSurface
     t <- curTime
     evalStateT (loop screen r) (initS t)


loop s (Sig (done -> Just (h :| t)))  = lift (drawBoxes s h) >> loop s t
loop s (Sig (done -> Just (End a)))  = return a
loop s (Sig r@(Await p c)) = upState p >>= loop s . Sig . update r

upState :: SDLPreds -> StateT SDLVals IO SDLVals
upState p = do 
             s <- get 
             t <- lift curTime
             let ps = hlookup Time p :: PredSet Seconds
             let mt = maxTime ps :: Seconds
             if mt < t 
             then
              do let s' = hmod Time (onV (const mt)) s 
                 put s'
                 return s'
             else 
              do evs <- lift getSDLEvs
                 let s'  = foldl handleEv s evs
                 let s'' = hmod Time (onV (const t)) s'
                 put s''
                 return s''
             

maxTime (PredSet ts) | null ms = 1.0 / 0
           | otherwise =  maximum ms
  where ms = mapFilter fromEq (elems ts)
        fromEq (Eq n) = Just n
        fromEq _       = Nothing

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f = catMaybes . map f

getSDLEvs = pollEvent >>= 
            \h -> case h of
              NoEvent -> return []
              Quit -> error "Quit!"
              _ -> do  t <- getSDLEvs
                       return (h:t)



handleEv :: SDLVals -> SDL.Event -> (SDLVars V)
handleEv l (MouseMotion x y _ _)    = hmod MousePos (onV (const (fromIntegral x :: Double,fromIntegral y :: Double))) l
handleEv l (MouseButtonDown _ _ x)  = hmod MouseBtns (onV (insert (toM x))) l
handleEv l (MouseButtonUp  _ _ x)   = hmod MouseBtns (onV (delete (toM x))) l
handleEv l _                        = l

curTime = do t <-  getTicks
             return ((fromIntegral t) / 1000.0)

toM ButtonLeft    = MLeft
toM ButtonMiddle  = MMiddle
toM ButtonRight   = MRight
toM _             = MMiddle

--}

