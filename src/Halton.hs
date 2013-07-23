import Data.Default
import Data.List
import Data.List.Utils
import System.Environment

type N = Integer
type R = Float

asR :: N -> R
asR = fromInteger . toInteger

-- Main> uniq (map (`div`2) (d primes))
-- [0,1,2,3,4,7,5,6,9,10,11,17,12,8,13,14,15,16,18,22,21,20,26,24,19,36,25,31,27,30,29,23,28,32,34,43,33,35,39,38,41,48,56,50,37,45,42,57,40,44,49,46,53,47,59,66,52,51,55,63,60,74,54,61,69,64,77,65,58^CInterrupted.

d :: Num a => [a] -> [a]
d (x:y:xs) = (y-x) : d (y:xs)
d _ = []

prime :: N -> N
prime = genericIndex primes

primes :: [N]
primes = sieve [2..]

sieve :: [N] -> [N]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

halton :: N -> N -> R
halton = generalizedHalton faure

halton' :: N -> N -> R
halton' = generalizedHalton (\d -> id)

generalizedHalton :: (N -> N -> N) -> N -> N -> R
generalizedHalton s d = phi (s b) b where b = (prime d)

phi :: (N -> N) -> N -> N -> R
phi s b k = sum [asR (s d) / asR q | (d,q) <- zip (poly b k) (iterate (b*) b)]

poly :: N -> N -> [N]
poly b 0 = [0]
poly b k = r : poly b q where (q,r) = divMod k b

faure :: N -> N -> N
faure 0 k = k
faure 1 k = k
faure b k = case divMod b 2 of
  (c,0) -> 2 * faure c r + q where (q,r) = divMod k c
  (c,1) -> case compare k c of
    EQ -> c
    LT -> if y < c then y else y+1 where y = faure (b-1) k
    GT -> if y < c then y else y+1 where y = faure (b-1) (k-1)

main = do
  print (map (`div`2) (d primes))
