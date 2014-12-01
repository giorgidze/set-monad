{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

import Data.Monoid
import Data.Set.Monad hiding (empty)
import qualified Data.Set.Monad as S
import Control.Applicative
import Control.Monad
import qualified Data.Foldable as Foldable

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = fmap fromList arbitrary

prop_fromList_toList :: Set Integer -> Bool
prop_fromList_toList x = fromList (toList x) == x

prop_monoid_law_1 :: Set Integer -> Bool
prop_monoid_law_1 x = mempty `mappend` x == x

prop_monoid_law_2 :: Set Integer -> Bool
prop_monoid_law_2 x = x `mappend` mempty == x

prop_monoid_law_3 :: Set Integer -> Set Integer -> Set Integer -> Bool
prop_monoid_law_3 x y z = (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)

prop_functor_law_1 :: Set Integer -> Bool
prop_functor_law_1 x = fmap id x == x

prop_functor_law_2 :: Set Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool
prop_functor_law_2 x f g = fmap (apply f . apply g) x == fmap (apply f) (fmap (apply g) x)

prop_applicative_law_1 :: Set Integer -> Fun Integer Integer -> Bool
prop_applicative_law_1 x f = (pure (apply f) <*> x) == fmap (apply f) x

prop_applicative_law_2 :: Set Integer -> Bool
prop_applicative_law_2 x = (pure id <*> x) == x

prop_applicative_law_3 :: Set (Integer -> Integer) -> Set (Integer -> Integer) -> Set Integer -> Bool
prop_applicative_law_3 x y z = (pure (.) <*> x <*> y <*> z) == (x <*> (y <*> z))

prop_applicative_law_4 :: Integer -> Fun Integer Integer -> Bool
prop_applicative_law_4 x f = ((pure (apply f) <*> pure x) :: Set Integer) == pure (apply f x)

prop_applicative_law_5 :: Set (Integer -> Integer) -> Integer -> Bool
prop_applicative_law_5 x y = (x <*> pure y) == (pure ($ y) <*> x)

prop_alternative_law_1 :: Set Integer -> Bool
prop_alternative_law_1 x = (empty <|> x) == x

prop_alternative_law_2 :: Set Integer -> Bool
prop_alternative_law_2 x = (x <|> empty) == x

prop_alternative_law_3 :: Set Integer -> Set Integer -> Set Integer -> Bool
prop_alternative_law_3 x y z = ((x <|> y) <|> z) == (x <|> (y <|> z))

prop_monad_law_1 :: Integer -> Fun Integer (Set Integer) -> Bool
prop_monad_law_1 x f = (return x >>= apply f) == apply f x

prop_monad_law_2 :: Set Integer -> Bool
prop_monad_law_2 m = (m >>= return) == m

prop_monad_law_3  :: Set Integer
                  -> Fun Integer (Set Integer)
                  -> Fun Integer (Set Integer)
                  -> Bool
prop_monad_law_3 m f g =
  ((m >>= apply f) >>= apply g) == (m >>= (\x -> apply f x >>= apply g))

prop_monad_plus_law_1 :: Set Integer -> Bool
prop_monad_plus_law_1 x = (mzero `mplus` x) == x

prop_monad_plus_law_2 :: Set Integer -> Bool
prop_monad_plus_law_2 x = (x `mplus` mzero) == x

prop_monad_plus_law_3 :: Set Integer -> Set Integer -> Set Integer -> Bool
prop_monad_plus_law_3 x y z = ((x `mplus` y) `mplus` z) == (x `mplus` (y `mplus` z))

prop_foldable :: Set Integer -> Bool
prop_foldable s = Foldable.foldr (+) 0 s == S.foldr (+) 0 s

main :: IO ()
main = do
  putStrLn "prop_fromList_toList"
  quickCheck prop_fromList_toList

  putStrLn "prop_monoid_law_1:"
  quickCheck prop_monoid_law_1
  putStrLn "prop_monoid_law_2:"
  quickCheck prop_monoid_law_2
  putStrLn "prop_monoid_law_3:"
  quickCheck prop_monoid_law_3

  putStrLn "prop_functor_law_1:"
  quickCheck prop_functor_law_1
  putStrLn "prop_functor_law_2:"
  quickCheck prop_functor_law_2

  putStrLn "prop_applicative_law_1:"
  quickCheck prop_applicative_law_1
  putStrLn "prop_applicative_law_2:"
  quickCheck prop_applicative_law_2
  putStrLn "prop_applicative_law_4:"
  quickCheck prop_applicative_law_4

  putStrLn "prop_alternative_law_1:"
  quickCheck prop_alternative_law_1
  putStrLn "prop_alternative_law_2:"
  quickCheck prop_alternative_law_2
  putStrLn "prop_alternative_law_3:"
  quickCheck prop_alternative_law_3

  putStrLn "prop_monad_law_1:"
  quickCheck prop_monad_law_1
  putStrLn "prop_monad_law_2:"
  quickCheck prop_monad_law_2
  putStrLn "prop_monad_law_3:"
  quickCheck prop_monad_law_3

  putStrLn "prop_monad_plus_law_1:"
  quickCheck prop_monad_plus_law_1
  putStrLn "prop_monad_plus_law_2:"
  quickCheck prop_monad_plus_law_2
  putStrLn "prop_monad_plus_law_3:"
  quickCheck prop_monad_plus_law_3
  
  putStrLn "prop_foldable:"
  quickCheck prop_foldable
