module Webb.Async.Data.Vector where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)


{- Provide proper language for the inner operations we want to perform. It is NOT 
  best-provided by the Array, since we may want to vary the actual type in the end.
-}

newtype Vector a = V (Array a)

derive newtype instance (Eq a) => Eq (Vector a)
derive newtype instance (Ord a) => Ord (Vector a)
derive newtype instance Show a => Show (Vector a)
derive newtype instance Semigroup (Vector a)
derive newtype instance Monoid (Vector a)
derive newtype instance Functor Vector
derive newtype instance Apply Vector
derive newtype instance Applicative Vector
derive newtype instance Bind Vector
derive newtype instance Monad Vector
derive instance Newtype (Vector a) _

fromArray :: forall a. Array a -> Vector a
fromArray = wrap

toArray :: forall a. Vector a -> Array a
toArray = unwrap

addLast :: forall a. a -> Vector a -> Vector a
addLast a = unwrap >>> flip Array.snoc a >>> wrap

addFirst :: forall a. a -> Vector a -> Vector a
addFirst a = unwrap >>> Array.cons a >>> wrap

drop :: forall a. Int -> Vector a -> Vector a
drop n = unwrap >>> Array.drop n >>> wrap

dropEnd :: forall a. Int -> Vector a -> Vector a
dropEnd n = unwrap >>> Array.dropEnd n >>> wrap

take :: forall a. Int -> Vector a -> Vector a
take n = unwrap >>> Array.take n >>> wrap

takeEnd :: forall a. Int -> Vector a -> Vector a
takeEnd n = unwrap >>> Array.takeEnd n >>> wrap

head :: forall a. Vector a -> Maybe a
head = unwrap >>> Array.head

last :: forall a. Vector a -> Maybe a
last = unwrap >>> Array.last

reverse :: forall a. Vector a -> Vector a
reverse = unwrap >>> Array.reverse >>> wrap