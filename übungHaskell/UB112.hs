-- youssef kharita 
import Text.Show.Functions
data List a = Nil | Cons a (List a) deriving Show

list :: List Int
list = Cons (-3) (Cons 14 ( Cons (-6)(Cons 7 (Cons 1 Nil))))

blist :: List Int 
blist = Cons 1 (Cons 1 ( Cons 0 (Cons 0 Nil)))

filterList :: (a -> Bool) -> List a -> List a 
filterList f (Cons a b) = if f a then Cons a (filterList f b) else filterList f b
filterList f Nil =  Nil

divisibleBy :: Int -> List Int -> List Int
divisibleBy x xs = filterList (\n -> rem n x == 0) xs

foldList :: (a -> b -> b) -> b -> List a -> b
foldList f c Nil = c
foldList f c (Cons a b) = f a (foldList f c b)

listMaximum :: List Int -> Int
listMaximum b = foldList (\x y -> if x > y then x else y) minBound b 

mapList :: (a -> b) -> List a -> List b
mapList f l = foldList g Nil l 
             where g = \x -> Cons (f x)

zipLists :: (a -> b ->c) -> List a -> List b -> List c
zipLists f  (Cons a b) (Cons x y)= Cons (f a x) (zipLists f b y) 
zipLists f  Nil        _         = Nil           
zipLists f  _          Nil       = Nil


skalaprodukt :: List Int -> List Int -> Int
skalaprodukt a b = foldList (\x y -> x + y) 0 (zipLists (\x y -> x*y) a b) 

j :: Int
j = (\f g x -> if (g x) then 3 * (f x) else 4 + (f x)) (\x->1) even 8

i :: [Bool]
i = filter (\x->x) (map even [1,2,3,4,5,6])