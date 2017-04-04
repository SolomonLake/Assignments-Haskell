{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- Worked with Nathan Holeman

module Class where

import Data.Kind
import Prelude hiding ( reverse, (++), last, map, and, or, any, unzip, init)
import Data.Type.Equality

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)

stuff = 5 :> 3 :> 8 :> Nil

safeHead :: Vec (Succ n) a -> a
safeHead (x :> _) = x
-- NO!  safeHead Nil      = error "urk"

safeTail :: Vec (Succ n) a -> Vec n a
safeTail (_ :> xs) = xs

snoc :: Vec n a -> a -> Vec (Succ n) a
snoc Nil       x = x :> Nil
snoc (y :> ys) x = y :> snoc ys x

reverse :: Vec n a -> Vec n a
reverse Nil       = Nil
reverse (x :> xs) = snoc (reverse xs) x

reverseList :: [a] -> [a]
reverseList xs = go [] xs
  where
    go acc []     = acc
    go acc (y:ys) = go (y:acc) ys

-- reverseVec :: Vec n a -> Vec n a
-- reverseVec xs = go SZero Nil xs
--   where
--     go :: SNat m -> Vec m a -> Vec p a -> Vec (m + p) a
--     go len acc Nil     = case plus_right_id len of Refl -> acc
--     go len acc (y:>ys) = go (SSucc len) (y :> acc) ys

type family Plus (a :: Nat) (b :: Nat) :: Nat where
  Plus Zero     b = b
  Plus (Succ a) b = Succ (Plus a b)

type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)
infixl 6 +

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ v2 = v2
(x :> xs) ++ v2 = x :> (xs ++ v2)

map :: (a -> b) -> Vec n a -> Vec n b
map f Nil = Nil
map f (x :> xs) = f x :> map f xs

last :: Vec (Succ n) a -> a
last (x :> Nil) = x
last (_ :> xs@(_ :> _)) = last xs

and :: Vec n Bool -> Bool
and Nil = True
and (x :> xs) =
  case x of
    False -> False
    True  -> and xs

or :: Vec n Bool -> Bool
or Nil = False
or (x :> xs) =
  case x of
    False -> or xs
    True  -> True

any :: (a -> Bool) -> Vec n a -> Bool
any f Nil = False
any f (x :> xs) =
  case (f x) of
    False -> any f xs
    True -> True

unzip :: Vec n (a, b) -> (Vec n a, Vec n b)
unzip Nil = (Nil, Nil)
unzip ((x,y) :> xs) = (x :> fst (unzip xs), y :> snd (unzip xs))

uncons :: Vec (Succ n) a -> (a, Vec n a)
uncons (x :> xs) = (x,xs)

{-
init :: Vec n a -> Vec n a
init (x :> Nil) = Nil
init (x :> xs)  = x :> init xs


insert :: Ord a => a -> Vec (Succ n) a -> Vec n a
insert elem (x :> xs) | elem <= x = elem :> xs
insert elem (x :> xs) | otherwise = insert elem xs
-}

sort :: Ord a => Vec n a -> Vec n a
sort 

-- singleton Nat
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

{-
data a :~: b where
  Refl :: a :~: a
-}

f :: (a :~: Int) -> a
f Refl = 5

plus_right_id :: SNat n -> (n + Zero) :~: n
plus_right_id SZero = Refl
plus_right_id (SSucc n')
  = case plus_right_id n' of
      Refl -> Refl
