import Seq
import Par
import Prelude hiding (map, filter, append, take, drop, scan, fromList)

instance Seq [] where
  emptyS = []
  singletonS x = [x]
  lengthS = length
  nthS = nth
  tabulateS = tabulate
  mapS = map
  filterS = filter
  appendS = append
  takeS = take
  dropS = drop
  showtS = showt
  showlS = showl
  joinS = join
  reduceS = reduce
  scanS = scan
  fromList = id

nth :: [a] -> Int -> a
nth (x:_) 0  = x
nth (_:xs) n = nth xs (n-1)

tabulate :: (Int -> a) -> Int -> [a]
tabulate f 0 = [f 0]
tabulate f n = tabulate' f 0 (n - 1)

tabulate' f y n | y == n = [f n]
                | otherwise = let (x, xs) = f y ||| tabulate' f (y+1) n
                              in x:xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = let (y, ys) = f x ||| map f xs
               in y:ys

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter f (x:xs) = let (bool, ys) = f x ||| filter f xs
                  in if bool then x:ys else ys

append :: [a] -> [a] -> [a]
append [] ys     = ys
append _ []      = []
append (x:xs) ys = x:(append xs ys)

take :: [a] -> Int -> [a]
take _ 0      = []
take [] _     = []
take (x:xs) n = x:(take xs (n-1))

drop :: [a] -> Int -> [a]
drop xs 0 = xs
drop [] _ = []
drop (x:xs) n = drop xs (n - 1)

showt :: [a] -> TreeView a [a]
showt [] = EMPTY
showt [x] = ELT x
showt xs = let len = length xs
               mid = div len 2
               (l, r) = take xs mid ||| drop xs mid
           in NODE l r

showl :: [a] -> ListView a [a]
showl [] = NIL
showl (x:xs) = CONS x xs

join :: [[a]] -> [a]
join [] = []
join (xs:yss) = append xs (join yss)

contract :: (a -> a -> a) -> [a] -> [a]
contract _ [] = []
contract _ l@[x] = l
contract f (x:y:xs) = let (z, zs) = f x y ||| contract f xs
                      in z : zs

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce _ e [] = e
reduce f e [x] = f e x
reduce f e xs = let ys = contract f xs
                in reduce f e ys

evenExpand :: (a -> a -> a) -> [a] -> [a] -> [a]
evenExpand _ _ [] = []
evenExpand _ [] _ = []
evenExpand f xs l@(y:ys) = y : (oddExpand f xs l)

oddExpand :: (a -> a -> a) -> [a] -> [a] -> [a]
oddExpand _ _ [] = []
oddExpand _ [] _ = []
oddExpand _ [x] _ = []
oddExpand f (x:_:xs) (y:ys) = (f y x) : (evenExpand f xs ys)

scan :: (a -> a -> a) -> a -> [a] -> ([a], a)
scan _ e [] = ([e], e)
scan f e [x] = ([e], f e x)
scan f e xs = let (ys, x) = scan f e (contract f xs)
                  zs = evenExpand f xs ys
              in (zs, x)