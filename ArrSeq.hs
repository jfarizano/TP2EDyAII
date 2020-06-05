import Seq
import qualified Arr as A
import Arr ((!))
import Par
import Prelude hiding (map, filter, take, drop, reduce, scan)

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

reduce :: (a -> a -> a) -> a -> A.Arr a -> a
reduce f e a = undefined

scan :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scan = undefined