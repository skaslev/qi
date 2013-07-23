{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
import Data.Default
import Test.QuickCheck
import Test.QuickCheck.All

fact n
  | n == 0 = 1
  | otherwise = n * fact (n-1)

fact1 0 = 1
fact1 n = n * fact1 (n-1)

fact2 n =
  if n == 0 then 1 else n * fact2 (n-1)

prop_FactEqFact2 n =
  if n < 0 then True
  else
    let f = (fact n)
        f1 = (fact1 n)
        f2 = (fact2 n)
    in f == f1 && f == f2

main = $quickCheckAll
