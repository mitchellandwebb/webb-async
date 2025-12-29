module Webb.Internal.Mutex.State where

import Prelude
import Webb.State.Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Webb.Data.Mutex.Id (Id)
import Webb.Data.Mutex.Id as Id
import Webb.Data.Mutex.Lease (Lease)
import Webb.Data.Mutex.Lease as Lease
import Webb.Data.Mutex.Queue (Queue)
import Webb.Data.Mutex.Queue as Queue


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
  
size :: MutexState -> Aff Int
size s = Queue.size <: s.queue

isLocked :: MutexState -> Aff Boolean
isLocked this = do 
  Lease.isOwned <: this.lease

isLockedByName :: MutexState -> String -> Aff Boolean
isLockedByName this name = do 
  Lease.isOwnedBy name <: this.lease

isLockedById :: MutexState -> Id -> Aff Boolean
isLockedById this id = do 
  Lease.hasId id <: this.lease