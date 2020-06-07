import Seq
import qualified Arr as A
import Arr ((!))
import Par
import Prelude hiding (map, filter, take, drop, reduce, scan)

-- Reemplar ! por nthS

instance Seq A.Arr where
  emptyS = A.empty
  singletonS x = A.fromList [x]
  lengthS = A.length
  nthS a n = a ! n
  tabulateS = A.tabulate
  mapS = map
  filterS = filter
  appendS = append
  takeS = take
  dropS = drop
  showtS = showt
  showlS = showl
  joinS = A.flatten
  reduceS = reduce
  scanS = undefined
  fromList = A.fromList

map :: (a -> b) -> A.Arr a -> A.Arr b
map f a = tabulateS (f . \i -> (a ! i)) n
          where n = lengthS a

filter :: (a -> Bool) -> A.Arr a -> A.Arr a
filter f a = case n of
              1 -> if f (a ! 0) then a else emptyS
              _ -> let 
                    mid = div n 2
                    (l, r) = (filter f (take a mid)) ||| (filter f (drop a mid))
                    in append l r
              where n = lengthS a
                    
append :: A.Arr a -> A.Arr a -> A.Arr a
append a b = let
             sa = lengthS a
             sb = lengthS b
             n = sa + sb
             in tabulateS (id . \i -> if i < sa then a ! i else b ! (i - sa)) n

take :: A.Arr a -> Int -> A.Arr a
take a n = A.subArray 0 (min n (lengthS a)) a

drop :: A.Arr a -> Int -> A.Arr a
drop a n = if n >= size then emptyS 
                        else A.subArray n (size - n) a
           where size = lengthS a

showt :: A.Arr a -> TreeView a (A.Arr a)
showt a = case n of
            0 -> EMPTY
            1 -> ELT (a ! 0)
            _ -> let  mid = div n 2
                      (l, r) = take a mid ||| drop a mid
                 in NODE l r
            where n = lengthS a

showl :: A.Arr a -> ListView a (A.Arr a)
showl a = case n of
            0 -> NIL
            _ -> CONS (a ! 0) (drop a 1)
            where n = lengthS a

contract :: (a -> a -> a) -> A.Arr a -> A.Arr a
contract f a = case n of
                  1 -> a
                  _ -> let  
                        x = tabulateS (\i -> f (a ! (2 * i)) (a ! (2 * i + 1))) (div n 2)
                        in if even n then x else append x (drop a (n - 1))
                where n = lengthS a

reduce'    :: (a -> a -> a) -> a -> A.Arr a -> a
reduce' f e xs = let t = showtS xs in case t of 
                                      EMPTY     -> e
                                      ELT x     -> x
                                      NODE l r -> f (reduce' f e l) (reduce' f e r)

reduce :: (a -> a -> a) -> a -> A.Arr a -> a
reduce f e a = case n of
                0 -> e
                1 -> a ! 0
                _ -> reduce f e (contract f a)
                where n = lengthS a

-- scan :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
-- scan f e a = case n of
--               0 -> (singletonS e, e)
--               1 -> (a, a ! 0)
--               _ -> let 
--                     (b, x) = scan f e (contract f e a)
--                     in tabulate (\i -> if even i then b !
--             where n = lengthS a

-- contract :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a
-- contract f e a = case n of
--                  1 -> a
--                  _ -> let (b, bs) = (f (a ! 0) (a ! 1)) ||| (contract f e (dropS a 2))
--                       in appendS (singletonS b) bs
--                 where n = lengthS a

-- [1, 2] [3] [4, 5] [6]
-- [1, 2] [3, 4] [5, 6] 

-- [1, 2, 3, 4, 5, 6] -> [1 - 2, 3 -4, 5 - 6] = [-1, -1, -1] -> [0, -1] -> [1]
-- [1, 2, 3, 4, 5, 6] +

-- (1 + 2) + (3 + 4) + (5 + 6)

-- tabulate 3 f --> 

-- n = 3, 

-- f i = a[2i] + a[2i+1] i = 0...2

-- i = 0 -> a0 + a1
-- i = 1 -> a2 + a3
-- i = 2 -> a4 + a5

-- f i = if i = n/2 - 1 then last
--       else a[2i] + a[2i+1]

-- g :: (a -> a -> a) -> A.Arr a -> Int -> a
-- g f a i = 