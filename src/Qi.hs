-- Quantum statics // Primus quantum valeō
-- Author: Slavomir Kaslev <slavomir.kaslev@gmail.com>, 2013

{-# LANGUAGE GADTs #-}
import Data.Complex
import Data.List hiding ((!!), replicate)
import Data.List.Utils (uniq)
import qualified Data.MultiSet as MS
import Data.Number.Nat (Nat(..))
import Data.Number.Nat1 (toNat1)
import Prelude hiding ((!!), replicate)

(!!) = genericIndex
replicate = genericReplicate

type N = Integer
type C = Complex Float

sieve :: [N] -> [N]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

primes :: [N]
primes = sieve [2..]

data Prime
  where Prime :: Nat -> Prime
  deriving Show

prs :: [Prime]
prs = [Prime (Pos (toNat1 p)) | p <- primes]

data Qi a
  where Qi :: Nat -> Prime -> a -> Qi a
  deriving Show

instance Eq (Qi a) where
  (Qi i _ _) == (Qi j _ _) = i == j

instance Ord (Qi a) where
  compare (Qi i _ _)  (Qi j _ _) = compare i j

vacuum :: Qi C
vacuum = Qi Zero undefined (0 :+ 0)

one :: Qi C
one = Qi (Pos 1) undefined (1 :+ 0)

qprimes :: [Qi C]
qprimes = [Qi (Pos $ toNat1 i) p (1 :+ 0) | (i, p) <- zip [2..] prs]

qi :: [Qi C]
qi = vacuum : one : qprimes

data Spectrum a
  where
    Black :: Spectrum a
    Factors :: MS.MultiSet (Qi a) -> Spectrum a
  deriving (Eq, Ord, Show)

spectrum :: [Spectrum C]
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
