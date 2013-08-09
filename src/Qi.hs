-- Quantum statics
-- Author: Slavomir Kaslev <slavomir.kaslev@gmail.com>, 2013

{-# LANGUAGE GADTs #-}
import Data.Complex
import Data.List hiding ((!!), replicate)
import Data.List.Utils (uniq)
import Data.Maybe
import qualified Data.MultiSet as MS
import Data.Number.Nat (Nat(..))
import Data.Number.Nat1 (toNat1)
import Prelude hiding ((!!), replicate)

(!!) = genericIndex
replicate = genericReplicate

type N = Int --Integer
type C = Complex Float

-- Primus quantum valeō
data Prim a where
  Prim :: a -> N -> Prim a
  deriving (Eq, Show)

prims :: [Prim C]
prims = map (Prim (1.0:+0.0)) primes
-- pj :: Prime k -> Prime (k-1)
-- pk :: Prime (k-1) -> k

--data List a = Nil | Const a (List a)
--	Nil +  a Nil + a a Nil + a

data Ln a where
  Ln :: a -> Ln a

--lnp :: Prim a -> N
--lnp (Prim n) = n

-- cut :: N -> Prim -> Powers Prim
cut :: N -> N -> [N]
cut n p = takeWhile (\pn -> n `mod` pn == 0) (iterate (*p) p)
-- NOTE n and p are separable
-- |cut n| * (Powers Prim := iterate (*p) p)
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
  where Qi :: N -> Prim a -> a -> Qi a
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
qprimes = [Qi i p (1 :+ 0) | (i, p) <- zip [2..] prims]

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

sieve :: [N] -> [N]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

primes :: [N]
primes = sieve [2..]

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

d2 xs =
  map (\(p, q, r) -> p - 2 * q + r) $
  zip3 xs (drop 1 xs) (drop 2 xs)
-- Yuk!
psum xs = [sum (take k xs) | k<-[0..]]

-- A014321  The next new gap between successive odd primes (divided by 2).
-- *Main> uniq $ map ((`div`2) . fromIntegral) $ (d) primes
-- [0,1,2,3,4,7,5,6,9,10,11,17,12,8,13,14,15,16,18,22,21,20,26,24,19,36,25,31,27,30,29,23,28,32,34,43,33,35,39,38,41,48,56,50,37,45,42,57,40,44,49,46,53,47^CInterrupted.
--
-- *Main> uniq $ map ((`div`2) . fromIntegral) $ (d.d) primes
-- [0,1,-1,-2,2,5,-5,4,-4,3,8,-6,-8,6,-7,-10,14,-14,-3,11,7,10,13,-13,-11,-12,12,-9,9,16,-16,-15,20,-20,15,-19,-17,23,19,25,-23,17,34,-32,-22,29,-29,-24,-25,24,26,-27,22,-26,18,-21,28,-18,-28,21,31,-31,-41,27,-34,35,-30,40,-38,38,30,-35,46,49,-49,43,44,-40,37,36,-33,52,-45,32,-44,47,41,-37,33,42,-42,-39,-43,-36,39,-46
--
-- A073051  Least k such that Sum(i=1..k, prime_i + prime_{i+2} - 2prime_{i+1} ) = 2n+1.
-- In[7]:= NextPrim[n_Integer] :=
--  Block[{k = n + 1}, While[! PrimeQ[k], k++]; k];
--  a = Table[0, {50}];
--  s = 0;
--  k = 1;
--  p = 0;
--  q = 2;
--  r = 3;
--  While[k < 10^6,
--    p = q;
--    q = r;
--    r = NextPrim[q];
--    s = s + p + r - 2 q;
--  If[s < 101 && a[[(s + 1)/2]] == 0, a[[(s + 1)/2]] = k]; k++]; a
--
-- Out[7]= {1, 3, 8, 23, 33, 45, 29, 281, 98, 153, 188, 262, 366, 428, \
-- 589, 737, 216, 1182, 3301, 2190, 1878, 1830, 7969, 3076, 3426, 2224, \
-- 3792, 8027, 4611, 4521, 3643, 8687, 14861, 12541, 15782, 3384, 34201, \
-- 19025, 17005, 44772, 23282, 38589, 14356, 44902, 34214, 73320, 85786, \
-- 30801, 49413, 33607}
-- NOTE Very interesting! Integral (partial sum) over the 2nd derivative (instead of 1st).
-- *Main> [fromJust $ elemIndex k (map (`div`2) $ d primes) | k <- [1..50] ++ [53,56,57]]
-- [1,3,8,23,33,45,29,281,98,153,188,262,366,428,589,737,216,1182,3301,2190,1878,1830,7969,3076,3426,2224,3792,8027,4611,4521,3643,8687,14861,12541,15782,3384,34201,19025,17005,44772,23282,38589,14356,44902,34214,73320,85786,30801,49413,33607,85632,31544,40932]
--
-- *Main> [fromJust $ elemIndex k (map (`div`2) $ (d.d) primes) | k <- [-40..40]]
-- [34214,67179,23282,59256,73694,28592,15782,40025,3384,12541,19723,3643,10228,4521,4611,3794,3792,3076,3426,7233,1830,1878,8268,2190,1228,1662,216,428,707,445,188,833,153,179,98,29,33,241,8,3,0,2,9,95,32,28,178,293,97,1004,295,261,755,427,215,1845,1181,3085,4712,2342,1829,10775,4562,2223,4229,2808,4258,14370,7808,3642,24642,12540,45505,61786,3383,17004,40024,38588,24552,84141,23281]
