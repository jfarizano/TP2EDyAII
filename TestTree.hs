module TestTree where

data Tree a = E | Leaf a | Join (Tree a) (Tree a)

joinT :: Tree a -> Tree a -> Tree a
joinT l r = Join l r

l2Tree :: [a] -> [Tree a]
l2Tree [] = []
l2Tree (x:xs) = (Leaf x) : l2Tree xs

instance Show a => Show (Tree a) where
         show p = show' p
            where show' E = "empty"
                  show' (Leaf x) = show x
                  show' (Join l r) = "(" ++ show' l ++ " (+) " ++ show' r ++ ")"