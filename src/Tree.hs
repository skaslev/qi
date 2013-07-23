data G a = Node a [G a] deriving (Eq, Show)

leaf = flip Node []
branch = Node
val (Node x _) = x
children (Node _ gs) = gs
