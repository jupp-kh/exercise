-- youssef kharita 
import Text.Show.Functions
import Data.List
data Bintree a b = Blatt b | Knoten a (Bintree a b) (Bintree a b) deriving Show 

example :: Bintree (Int ->Bool) Char
example = Knoten (\x -> x > 4) (
	                 Knoten (\x -> x * x == x) (
	                 	Blatt 'g') (
	                 	Knoten (\x -> x == 0) (
	                 	Blatt 'u') (Blatt 'l'))) (
                        Knoten (\x -> x >= 7) (
                        Blatt 'f') (Blatt 'i'))

countInnerNodes :: (Bintree a b) -> Int
countInnerNodes (Knoten a b c)  = 1 + countInnerNodes b +countInnerNodes c
countInnerNodes (Blatt a) = 0

decodeInt :: (Bintree (Int -> Bool) b) -> Int -> b
decodeInt (Knoten x y z) n = if (x n) then decodeInt z n else decodeInt y n
decodeInt (Blatt c)      _ = c

decode :: (Bintree (Int -> Bool) b) -> [Int] ->[b]
decode bt (x:xs) = decodeInt bt x : decode bt xs 
decode bt  []    = []

mapTree ::  (b -> c) -> Bintree a b -> Bintree a c
mapTree f (Knoten x y z) = Knoten x (mapTree f y) (mapTree f z)
mapTree f (Blatt b) = Blatt (f b)

strings :: Int -> [String]
strings 0 = [""]
strings n = concat (map (\x -> map (\tail -> x:tail) tails) ['a'..'z'] )
          where tails = strings (n-1)

palindrome :: [String] -> [String]
palindrome [] = []
palindrome (a:as) = if a == reverse a then a : palindrome as else palindrome as

perfektenZahlen :: Int ->[Int]
perfektenZahlen x = if sum (divisors x) == x then x : perfektenZahlen(x+1) else perfektenZahlen(x+1)

divisors :: Int -> [Int]
divisors x = filter (\y -> rem x y == 0) [1..div x 2]

semiperfekt :: Int -> [Int]
semiperfekt x = if any (\a -> sum a == x) (subsequences (divisors x)) then x : semiperfekt (x +1) else semiperfekt(x+1) 

fiblist :: [Int]
fiblist = fibInit 0 1  
               
fibInit :: Int -> Int -> [Int]
fibInit a0 a1 = a0 : fibInit a1 (a1+a0)

