module Webb.Async.Data.Mutex.Id where

import Prelude

import Data.Int (rem)
import Data.Newtype (class Newtype, unwrap, wrap)




newtype Id = I Int


derive newtype instance Eq Id
derive newtype instance Ord Id
derive newtype instance Show Id
derive instance Newtype Id _

next :: Id -> Id
next = unwrap >>> inc >>> wrap
  where
  inc id = rem (id + 1) 100000
  
initial :: Id 
initial = wrap 0