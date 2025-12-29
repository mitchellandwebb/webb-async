module Webb.Internal.Mutex where

import Prelude
import Webb.State.Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff, finally)
import Webb.Internal.Mutex.Locker as Locker
import Webb.Internal.Mutex.State (MutexState)
import Webb.Internal.Mutex.State as State
import Webb.Internal.Mutex.Turn as Turn
import Webb.Internal.Mutex.Unlocker as Unlocker

type Mutex = MutexState

{- Implement the important mutex functions for the whole mutex. -}

-- Lock. Implemented in a model-based method, to summon the desired thoughts
-- while reading it.
lock :: Mutex -> Maybe String -> Aff Unit
lock mutex mname = do
  locker <- newLocker
  success <- Locker.lock locker mname
  unless success do 
    turn <- newTurn
    Turn.wait turn mname
    
  where
  newLocker = do pure $ Locker.newLocker mutex
  newTurn = do pure $ Turn.newTurn mutex
  
-- Unlock with the given name.
unlock :: Mutex -> Maybe String -> Aff Unit
unlock mutex mname = do 
  unlocker <- newUnlocker 
  Unlocker.unlock unlocker mname
  where
  newUnlocker = do pure $ Unlocker.newUnlocker mutex
  
locking :: forall a. Mutex -> Maybe String -> Aff a -> Aff a
locking mutex mname prog = do 
  lock mutex mname
  finally (unlock mutex mname) do
    prog
    
isLocked :: Mutex -> Aff Boolean
isLocked mutex = do State.isLocked mutex

isLockedBy :: Mutex -> String -> Aff Boolean
isLockedBy mutex name = do State.isLockedByName mutex name

