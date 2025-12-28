module Webb.Async.Data.Mutex where

import Prelude

import Data.Lens (Lens', over, set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, modify, unwrap, wrap)
import Type.Proxy (Proxy(..))
import Webb.Async.Data.Mutex.Id (Id)
import Webb.Async.Data.Mutex.Id as Id
import Webb.Async.Data.Mutex.Item (Item)
import Webb.Async.Data.Mutex.Item as Item
import Webb.Async.Data.Mutex.Lease (Lease)
import Webb.Async.Data.Mutex.Lease as Lease
import Webb.Async.Data.Vector (Vector)
import Webb.Async.Data.Vector as Vector
import Webb.State.Prelude (views)


{- Representation of the Mutex -}

newtype Mutex = M
  { id :: Id
  , lease :: Lease
  , queue :: Vector Item
  }

_id :: forall a r. Lens' { id :: a | r } a
_id = prop (Proxy :: Proxy "id")

_lease :: forall a r. Lens' { lease :: a | r } a
_lease = prop (Proxy :: Proxy "lease")

_queue :: forall a r. Lens' { queue :: a | r } a
_queue = prop (Proxy :: Proxy "queue")
  
derive newtype instance Eq Mutex
derive newtype instance Ord Mutex
derive newtype instance Show Mutex
derive instance Newtype Mutex _

newMutex :: Mutex 
newMutex = wrap 
  { id: Id.initial
  , lease: Lease.none
  , queue: Vector.empty
  }
  
currentId :: Mutex -> Id
currentId = unwrap >>> _.id

incId :: Mutex -> Mutex
incId = modify $ over _id Id.next

isOwningId :: Id -> Mutex -> Boolean
isOwningId id = unwrap >>> views _lease (Lease.hasId id)

isOwningName :: String -> Mutex -> Boolean
isOwningName name = unwrap >>> views _lease (Lease.isOwnedBy name)
  
isQueuedId :: Id -> Mutex -> Boolean
isQueuedId id = unwrap >>> views _queue (Vector.canFind (Item.hasTag id))
  
isOwned :: Mutex -> Boolean
isOwned = unwrap >>> views _lease Lease.isOwned
  
unsetLease :: Mutex -> Mutex
unsetLease = modify $ set _lease Lease.none

setLease :: Id -> Maybe String -> Mutex -> Mutex
setLease id name = modify $ set _lease (Lease.newLease id name)

setLeaseWithItem :: Item -> Mutex -> Mutex
setLeaseWithItem item = modify $
  set _lease (Lease.newLease (Item.tag item) (Item.name item))
  
hasNextItem :: Mutex -> Boolean
hasNextItem = unwrap >>> views _queue (Vector.isEmpty >>> not)

containsItemWith :: (Item -> Boolean) -> Mutex -> Boolean
containsItemWith f = unwrap >>> views _queue (Vector.canFind f)

containsItemWithTag :: Id -> Mutex -> Boolean
containsItemWithTag tag = containsItemWith (Item.hasTag tag)

removeTaggedItem :: Id -> Mutex -> Mutex
removeTaggedItem tag = modify $
  over _queue (Vector.reject (Item.hasTag tag))

enqueueItem :: Item -> Mutex -> Mutex
enqueueItem item = modify $ over _queue (Vector.addLast item)

peekItem :: Mutex -> Maybe Item
peekItem = unwrap >>> views _queue (Vector.first)
  
dequeueItem :: Mutex -> Mutex
dequeueItem = unwrap >>> over _queue (Vector.drop 1) >>> wrap