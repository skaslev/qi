-- Slavomir Kaslev, 07/22/13
{-# LANGUAGE GADTs #-}
import Data.Complex
import Data.List

type N = Integer
type C = Complex Float

(#) :: [a] -> N -> a
a # b = genericIndex a b

sieve :: [N] -> [N]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

prime :: [N]
prime = sieve [2..]

data Q a b
  where Qa :: a -> b -> Q a b
  deriving Show

q :: N -> Q N C
q n = Qa (prime # n) (1 :+ 0)

data Zero a b
  where Zero :: a -> b -> Zero a b
  deriving Show

zero :: Zero N C
zero = Zero 0 (0 :+ 0)

data One a b
  where One :: a -> b -> One a b
  deriving Show

one :: One N C
one = One 1 (1 :+ 0)

quant :: [N]
quant = 0 : 1 : prime
