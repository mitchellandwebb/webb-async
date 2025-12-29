module Webb.Mutex where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Internal.Mutex as Mutex
import Webb.Internal.Mutex.State (MutexState, newMutexState)
import Webb.Monad.Prelude (launch_)


newtype Mutex = M MutexState

newMutex :: forall m. MonadEffect m => m Mutex
newMutex = liftEffect do
  state <- newMutexState
  pure $ M state

run :: forall a m. MonadAff m => Mutex -> (MutexState -> Aff a) -> m a
run (M m) prog = liftAff do prog m

lock :: forall m. MonadAff m => Mutex -> m Unit
lock self = run self $ 
  \mutex -> Mutex.lock mutex Nothing

lock' :: forall m. MonadAff m => Mutex -> String -> m Unit
lock' self name = run self $ 
  \mutex -> Mutex.lock mutex (Just name)

unlock :: forall m. MonadEffect m => Mutex -> m Unit
unlock self = launch_ $ run self $ 
  \mutex -> Mutex.unlock mutex Nothing

unlock' :: forall m. MonadEffect m => Mutex -> String -> m Unit
unlock' self name = launch_ $ run self $ 
  \mutex -> Mutex.unlock mutex (Just name)

locking :: forall m a. MonadAff m => Mutex -> Aff a -> m a
locking self prog = run self $ 
  \mutex -> Mutex.locking mutex Nothing prog

locking' :: forall m a. MonadAff m => Mutex -> String -> Aff a -> m a
locking' self name prog = run self $ 
  \mutex -> Mutex.locking mutex (Just name) prog
  
isLocked :: forall m. MonadAff m => Mutex -> m Boolean
isLocked self = run self $ 
  \mutex -> Mutex.isLocked mutex

isLockedBy :: forall m. MonadAff m => Mutex -> String -> m Boolean
isLockedBy self name = run self $ 
  \mutex -> Mutex.isLockedBy mutex name