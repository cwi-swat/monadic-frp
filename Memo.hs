
module Memo where

import System.IO.Unsafe
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import System.Mem.Weak
import Debug.Trace

memo :: Ord a => (a -> b) -> (a -> b)

memo f = (\x -> unsafePerformIO (lookup x))
  where ref = unsafePerformIO (newIORef (f,Map.empty))
        cleanup k = do (_,m) <- readIORef ref
                       let m' = Map.delete k m
                       writeIORef ref (f,m')
        addKey k v = do w <- mkWeak k v (Just (cleanup k))
                        (_,m) <- readIORef ref
                        let m' = Map.insert k w m
                        writeIORef ref (f,m')
        lookup k = do (_,m) <- readIORef ref
                      let w = Map.lookup k m
                      if isJust w 
                       then do w' <- deRefWeak (fromJust w)
                               if isJust w' 
                                then return (fromJust w')
                                else  compute k
                       else compute k
        compute k = do addKey k v 
                       return v
             where v = f k
                                


         
