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
  where Prime :: N -> Prime
  deriving (Eq, Show)

--data List a = Nil | Const a (List a)
--	Nil +  a Nil + a a Nil + a

prs :: [Prime]
prs = [Prime p | p <- primes]

data Ln a where
  Ln :: a -> Ln a

lnp :: Prime -> N
lnp (Prime n) = n

-- cut :: N -> Prime -> Powers Prime
cut :: N -> N -> [N]
cut n p = takeWhile (\pn -> n `mod` pn == 0) (iterate (*p) p)
-- NOTE n and p are separable
-- |cut n| * (Powers Prime := iterate (*p) p)
--
-- *Main> take 5 $ map (cut (8*9)) primes
-- [[2,4,8],[3,9],[],[],[]]
--
-- Map factorizing tree for every n, e.g.
-- (1,24) _ (2,12) _ (2,6) _ (2,3) _ (3,1) __ []
--        |        |       \ (3,2) _ (2,1) __ []
--        |        \ (3,4) _ (2,2) _ (2,1) __ []
--        \ (3, 8) _ (2,4) _ (2,2) _ (2,1) __ []

data Qi a
  where Qi :: N -> Prime -> a -> Qi a
  deriving Show

instance Eq (Qi a) where
  (Qi i _ _) == (Qi j _ _) = i == j

instance Ord (Qi a) where
  compare (Qi i _ _)  (Qi j _ _) = compare i j

vacuum :: Qi C
vacuum = Qi 0 undefined (0 :+ 0)

one :: Qi C
one = Qi 1 undefined (1 :+ 0)

qprimes :: [Qi C]
qprimes = [Qi i p (1 :+ 0) | (i, p) <- zip [2..] prs]

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
