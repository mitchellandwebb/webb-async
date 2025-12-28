module Webb.Async.Data.Mutex.Queue where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, modify, unwrap, wrap)
import Webb.Async.Data.Mutex.Id (Id)
import Webb.Async.Data.Mutex.Item (Item)
import Webb.Async.Data.Mutex.Item as Item
import Webb.Async.Data.Vector as Vector

{- Queue operations that are limited to the Item. -}

newtype Queue = Q (Vector.Vector Item)

derive newtype instance Show Queue
derive newtype instance Eq Queue
derive newtype instance Ord Queue
derive instance Newtype Queue _

fromArray :: Array Item -> Queue
fromArray arr = wrap $ Vector.fromArray arr

toArray :: Queue -> Array Item
toArray = unwrap >>> Vector.toArray

empty :: Queue
empty = wrap $ Vector.empty

removeId :: Id -> Queue -> Queue
removeId id = modify $ Vector.reject (Item.hasTag id)

peek :: Queue -> Maybe Item
peek = unwrap >>> Vector.first

dequeue :: Queue -> Queue
dequeue = modify $ Vector.drop 1

enqueue :: Item -> Queue -> Queue
enqueue item = modify $ Vector.addLast item