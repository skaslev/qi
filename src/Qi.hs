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

data Qi a b
  where Qi :: N -> a -> b -> Qi a b
  deriving Show

vacuum :: Qi N C
vacuum = Qi 0 0 (0 :+ 0)

one :: Qi N C
one = Qi 1 1 (1 :+ 0)

qprimes :: [Qi N C]
qprimes = [Qi i p (1 :+ 0) | (i,p) <- zip [2..] primes]

qi :: [Qi N C]
qi = vacuum : one : qprimes

data MSet a
  --deriving (Eq, Ord, Show)

data Spectrum a b
  where
    Black :: Spectrum a b
    Factors :: MSet (Qi a b) -> Spectrum a b
  --deriving (Eq, Ord, Show)

spectrum :: [Spectrum N C]
spectrum = undefined
--spectrum = Black : (Factors one []) : (Factors one qi)
-- TODO: needs normalization to |one|

d :: Num a => [a] -> [a]
d (x:y:xs) = (y-x) : d (y:xs)
d _ = []

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
