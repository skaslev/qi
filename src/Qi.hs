-- Quantum statics
{-# LANGUAGE GADTs #-}
import Data.Complex
import Data.List hiding ((!!), replicate)
import Prelude hiding ((!!), replicate)

(!!) = genericIndex
replicate = genericReplicate

type N = Integer
type C = Complex Float

sieve :: [N] -> [N]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

prime :: [N]
prime = sieve [2..]

data Q a b
  where Qi :: a -> b -> Q a b
  deriving Show

q :: N -> Q N C
q i = Qi (prime !! i) (1 :+ 0)

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

quant :: [N]
quant = 0 : 1 : prime

d :: Num a => [a] -> [a]
d (x:y:xs) = (y-x) : d (y:xs)
d _ = []


-- Natural numbers
data Nat a b
  where
    Zero :: Vacuum a b -> Nat a b
    Factors :: One a b -> [Q a b] -> Nat a b
    --
    -- Vacuum -> Nat
    -- One -> VectorSpace Q -> Nat

factor :: N -> [N]
factor 0 = []
factor 1 = []
factor n =
  (\(p:ps) ->
    let k = dlog p n
    in (replicate k p) ++ (factor (n `div` (p^k)))) $
  dropWhile (\p -> n `mod` p /= 0) $
  takeWhile (<= n) $
  prime

-- p <- prime
dlog :: N -> N -> N
dlog p n | r /= 0 = 0
         | otherwise = 1 + dlog p q
  where
    (q,r) = divMod n p

--factor :: N -> Cantor
--factor = epsilon is_factor
--factor :: N -> Dist  -- N -> [N]
--factor = [f i | i <-

type Cantor = N -> Bool
type Pred = Cantor -> Bool

epsilon :: Pred -> Cantor
epsilon p = branch x l r
 where
  branch x l r n
    | n == 0 = x
    | odd n = l ((n-1) `div` 2)
    | otherwise = r ((n-2) `div` 2)
  x = exists (\l -> (exists (\r -> p (branch True l r))))
  l = epsilon (\l -> (exists (\r -> p (branch x l r))))
  r = epsilon (\r -> p (branch x l r))

exists :: Pred -> Bool
exists p = p (epsilon p)
