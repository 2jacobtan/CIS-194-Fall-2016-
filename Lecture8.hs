data Tree a
    = Node [Tree a] -- a different tree!
    | Leaf a

instance Functor Tree where
    fmap f (Node ts) = Node (map (fmap f) ts)
    fmap f (Leaf x)  = Leaf (f x)