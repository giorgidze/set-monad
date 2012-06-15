{-# LANGUAGE GADTs #-}

module Data.Set.Monad (
  -- * Set type
  Set
  -- * Operators
  , (\\)

  -- * Query
  , null
  , size
  , member
  , notMember
  , isSubsetOf
  , isProperSubsetOf

  -- * Construction
  , empty
  , singleton
  , insert
  , delete

  -- * Combine
  , union
  , unions
  , difference
  , intersection

  -- * Filter
  , filter
  , partition
  , split
  , splitMember

  -- * Map
  , map
  , mapMonotonic

  -- * Folds
  , foldr
  , foldl
  -- ** Strict folds
  , foldr'
  , foldl'
  -- ** Legacy folds
  , fold

  -- * Min\/Max
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax
  , maxView
  , minView

  -- * Conversion

  -- ** List
  , elems
  , toList
  , fromList

  -- ** Ordered list
  , toAscList
  , fromAscList
  , fromDistinctAscList

  -- * Debugging
  , showTree
  , showTreeWith
  , valid
  ) where

import Prelude hiding (null, filter, map, foldr, foldl)
import qualified Data.List            as L
import qualified Data.Set             as S
import qualified Data.Functor         as F
import qualified Control.Applicative  as A

import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.DeepSeq

data Set a where
  Prim   :: (Ord a) => S.Set a -> Set a
  Return :: a -> Set a
  Bind   :: Set a -> (a -> Set b) -> Set b
  Zero   :: Set a
  Plus   :: Set a -> Set a -> Set a

instance F.Functor Set where
  fmap = liftM

instance A.Applicative Set where
  pure  = return
  (<*>) = ap

instance A.Alternative Set where
  empty = mzero
  (<|>) = mplus

instance Monad Set where
  return = Return
  (>>=)  = Bind

instance MonadPlus Set where
  mzero = Zero
  mplus = Plus

instance (Ord a) => Monoid (Set a) where
  mempty  = empty
  mappend = union
  mconcat = unions

instance (Ord a) => Eq (Set a) where
  s1 == s2 = run s1 == run s2

instance (Ord a) => Ord (Set a) where
  compare s1 s2 = compare (run s1) (run s2)

instance (Show a, Ord a) => Show (Set a) where
  show = show . run

instance (Read a, Ord a) => Read (Set a) where
  readsPrec i s = L.map (first Prim) (readsPrec i s)

instance (NFData a, Ord a) => NFData (Set a) where
  rnf = rnf . run

run :: (Ord a) => Set a -> S.Set a
run (Prim s)              = s
run (Return a)            = S.singleton a
run (Zero)                = S.empty
run (Plus ma mb)          = S.union (run ma) (run mb)
run (Bind (Prim s) f)     = S.foldl' S.union S.empty (S.map (run . f) s)
run (Bind (Return a) f)   = run (f a)
run (Bind Zero _)         = S.empty
run (Bind (Plus ma mb) f) = run (Plus (Bind ma f) (Bind mb f))
run (Bind (Bind m f) g)   = run (Bind m (\a -> Bind (f a) g))

infixl 9 \\

(\\) :: (Ord a) => Set a -> Set a -> Set a
m1 \\ m2 = difference m1 m2

null :: (Ord a) => Set a -> Bool
null = S.null . run 

size :: (Ord a) => Set a -> Int
size = S.size . run

member :: (Ord a) => a -> Set a -> Bool
member a s = S.member a (run s)

notMember :: (Ord a) => a -> Set a -> Bool
notMember a t = not (member a t)

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf s1 s2 = S.isSubsetOf (run s1) (run s2)

isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf s1 s2 = S.isProperSubsetOf (run s1) (run s2)

empty :: (Ord a) => Set a
empty = Prim S.empty

singleton :: (Ord a) => a -> Set a
singleton a = Prim (S.singleton a)

insert :: (Ord a) => a -> Set a -> Set a
insert a s = Prim (S.insert a (run s))

delete :: (Ord a) => a -> Set a -> Set a
delete a s = Prim (S.delete a (run s))

union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = Prim (S.union (run s1) (run s2))

unions :: (Ord a) => [Set a] -> Set a
unions ss = Prim (S.unions (L.map run ss))

difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = Prim (S.difference (run s1) (run s2))

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = Prim (S.intersection (run s1) (run s2))

filter :: (Ord a) => (a -> Bool) -> Set a -> Set a
filter f s = Prim (S.filter f (run s))

partition :: (Ord a) => (a -> Bool) -> Set a -> (Set a,Set a)
partition f s = (Prim *** Prim) (S.partition f (run s))

split :: (Ord a) => a -> Set a -> (Set a,Set a)
split a s = (Prim *** Prim) (S.split a (run s))

splitMember :: (Ord a) => a -> Set a -> (Set a, Bool, Set a)
splitMember a s = (\(s1,b,s2) -> (Prim s1,b,Prim s2)) (S.splitMember a (run s))

map :: (Ord a,Ord b) => (a -> b) -> Set a -> Set b
map f s = Prim (S.map f (run s))

mapMonotonic :: (Ord a,Ord b) => (a -> b) -> Set a -> Set b
mapMonotonic f s = Prim (S.mapMonotonic f (run s))

foldr :: (Ord a) => (a -> b -> b) -> b -> Set a -> b
foldr f z s = S.foldr f z (run s)

foldl :: (Ord a) => (b -> a -> b) -> b -> Set a -> b
foldl f z s = S.foldl f z (run s)

foldr' :: (Ord a) => (a -> b -> b) -> b -> Set a -> b
foldr' f z s = S.foldr' f z (run s)

foldl' :: (Ord a) => (b -> a -> b) -> b -> Set a -> b
foldl' f z s = S.foldl' f z (run s)

fold :: (Ord a) => (a -> b -> b) -> b -> Set a -> b
fold = foldr

findMin :: (Ord a) => Set a -> a
findMin = S.findMin . run

findMax :: (Ord a) => Set a -> a
findMax = S.findMax . run

deleteMin :: (Ord a) => Set a -> Set a
deleteMin = Prim . S.deleteMin . run

deleteMax :: (Ord a) => Set a -> Set a
deleteMax = Prim . S.deleteMax . run

deleteFindMin :: (Ord a) => Set a -> (a,Set a)
deleteFindMin s = second Prim (S.deleteFindMin (run s))

deleteFindMax :: (Ord a) => Set a -> (a,Set a)
deleteFindMax s = second Prim (S.deleteFindMax (run s))

maxView :: (Ord a) => Set a -> Maybe (a,Set a)
maxView = fmap (second Prim) . S.maxView . run

minView :: (Ord a) => Set a -> Maybe (a,Set a)
minView = fmap (second Prim) . S.minView . run

elems :: (Ord a) => Set a -> [a]
elems = toList

toList :: (Ord a) => Set a -> [a]
toList = S.toList . run

fromList :: (Ord a) => [a] -> Set a
fromList as = Prim (S.fromList as)

toAscList :: (Ord a) => Set a -> [a]
toAscList = S.toAscList . run

fromAscList :: (Ord a) => [a] -> Set a
fromAscList = Prim . S.fromAscList

fromDistinctAscList :: (Ord a) => [a] -> Set a
fromDistinctAscList = Prim . S.fromDistinctAscList

showTree :: (Show a,Ord a) => Set a -> String
showTree = S.showTree . run

showTreeWith :: (Show a, Ord a) => Bool -> Bool -> Set a -> String
showTreeWith b1 b2 s = S.showTreeWith b1 b2 (run s)

valid :: (Ord a) => Set a -> Bool
valid = S.valid . run