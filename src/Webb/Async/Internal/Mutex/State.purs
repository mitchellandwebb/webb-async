module Webb.Async.Internal.Mutex.State where

import Prelude

import Effect (Effect)
import Webb.Async.Data.Mutex.Id (Id)
import Webb.Async.Data.Mutex.Id as Id
import Webb.Async.Data.Mutex.Lease (Lease)
import Webb.Async.Data.Mutex.Lease as Lease
import Webb.Async.Data.Mutex.Queue (Queue)
import Webb.Async.Data.Mutex.Queue as Queue
import Webb.State.Prelude (ShowRef, newShowRef)


{- The basic shared state of a mutex. -}


type MutexState = 
  { lease :: ShowRef Lease
  , id :: ShowRef Id
  , queue :: ShowRef Queue
  }
  
newMutexState :: Effect MutexState
newMutexState = do
  lease <- newShowRef Lease.none
  id <- newShowRef Id.initial
  queue <- newShowRef Queue.empty
  pure { lease, id, queue }