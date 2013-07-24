-- Quantum statics // Primus quantum valeō
-- Author: Slavomir Kaslev <slavomir.kaslev@gmail.com>, 2013

{-# LANGUAGE GADTs #-}
import Data.Complex
import Data.List hiding ((!!), replicate)
import Data.List.Utils
import Prelude hiding ((!!), replicate)

(!!) = genericIndex
replicate = genericReplicate

type N = Integer
type C = Complex Float

sieve :: [N] -> [N]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

primes :: [N]
primes = sieve [2..]

data A a b
  where A :: a -> b -> A a b
  deriving Show

a :: N -> A N C
a i = A (primes !! i) (1 :+ 0)

data Vacuum a b
  where Vacuum :: a -> b -> Vacuum a b
  deriving Show

vacuum :: Vacuum N C
vacuum = Vacuum 0 (0 :+ 0)

data One a b
  where One :: a -> b -> One a b
  deriving Show

one :: One N C
one = One 1 (1 :+ 0)

-- spectrum
quanta :: [N]
quanta = 0 : 1 : primes

d :: Num a => [a] -> [a]
d (x:y:xs) = (y-x) : d (y:xs)
d _ = []


-- Natural numbers
-- Vacuum -> Nat
-- One -> VectorSpace (A a) -> Nat
data Nat a b
  where
    Zero :: Vacuum a b -> Nat a b
    Factors :: One a b -> [A a b] -> Nat a b

factor :: N -> [N]
factor 0 = []
factor 1 = []
factor n = replicate k p ++ factor (n `div` (p^k))
  where
    p = head $
        dropWhile (\p -> n `mod` p /= 0) $
        takeWhile (<= n) $
        primes
    k = dlog p n

dlog :: N -> N -> N
dlog b n | r /= 0 = 0
         | otherwise = 1 + dlog b q
  where (q,r) = n `divMod` b
