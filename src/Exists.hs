type Cantor = Int -> Bool

--(#) :: Bool -> Cantor -> Cantor
(x # a) 0 = x
(x # a) n = a (n-1)

--epsilon' :: (Cantor -> Bool) -> Cantor
epsilon' p =
 if exists' (\a -> p (False # a))
  then False # epsilon' (\a -> p (False # a))
  else True  # epsilon' (\a -> p (True # a))

--exists' :: (Cantor -> Bool) -> Bool
exists' p = p (epsilon' p)

--epsilon :: (Cantor -> Bool) -> Cantor
epsilon p = branch x l r
 where
  --branch :: Bool -> Cantor -> Cantor -> Int -> Bool
  branch x l r n
    | n == 0 = x
    | odd n = l ((n-1) `div` 2)
    | otherwise = r ((n-2) `div` 2)
  --x::Bool
  --l,r :: Cantor
  x = exists (\l -> (exists (\r -> p (branch True l r))))
  l = epsilon (\l -> (exists (\r -> p (branch x l r))))
  r = epsilon (\r -> p (branch x l r))

--exists :: (Cantor -> Bool) -> Bool
exists p = p (epsilon p)
