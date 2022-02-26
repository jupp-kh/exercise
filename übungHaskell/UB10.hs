-- Gruppe 5
-- Youssef Kharita 
fibInit :: Int -> Int -> Int -> Int
fibInit a0 a1 0 = a0
fibInit a0 a1 1 = a1
fibInit a0 a1 n = fibInit a0 a1 (n-2) + fibInit a0 a1 (n-1) 

fibInitL :: Int->Int->Int->[Int]
fibInitL a0 a1 0 = [a0]
fibInitL a0 a1 n| n < -1 = []
                | otherwise = fibInitL a0 a1 (n-1)++[fibInit a0 a1 n]

fibInit2 :: Int->Int->Int->Int
fibInit2 a0 a1 0 = a0
fibInit2 a0 a1 1 = a1
fibInit2 a0 a1 n = fibInit2 a1 (a0+a1) (n-1)

normalize :: [Int]->[Int]
normalize [] = []
normalize x  = differenz (pruferNegativ x) x

pruferNegativ :: [Int]-> Int
pruferNegativ [] = 0
pruferNegativ (x:xs) | x < 0 = x + pruferNegativ xs 
                     | x >= 0 = pruferNegativ xs
                 
differenz :: Int -> [Int] -> [Int]
differenz x [] = []
differenz x (y:ys) = [y-x] ++ differenz x ys 

sumMaxs :: [Int]-> Int
sumMaxs [] = 0
sumMaxs (x:xs) = x + sumMaxs(isMax x xs) 

isMax :: Int -> [Int]->[Int]
isMax x [] = []
isMax x (y:ys) | x >= y = []++ isMax x ys
               | otherwise = (y:ys)

sumNonMins :: [Int]->Int
sumNonMins [] = 0
sumNonMins (x:y:xs) | y > x = y + sumNonMins (x:xs) 
                    | otherwise = sumNonMins (y:xs)
sumNonMins (x:xs) = 0 

primeTwins ::Int -> (Int,Int)                   
primeTwins n | (prime n) && (prime (n+2)) = (n , n+2) 
             | otherwise = primeTwins (n+1)

prime :: Int -> Bool
prime 0 = False
prime 1 = False
prime 2 = True
prime n = primeTest n (n-1)

primeTest :: Int -> Int -> Bool
primeTest n m | m == 1 = True
              | (rem n m == 0) = False
              | otherwise = primeTest n (m-1)

multiples :: [Int] -> Int -> Int -> [Int]
multiples [] i0 i1 = []
multiples xs i0 i1 | i0 > i1 = []
                   | (i0 <= i1) && (dividierbar i0 xs) = [i0] ++ multiples xs (i0+1) i1 
                   | otherwise = multiples xs (i0+1) i1  

dividierbar :: Int -> [Int] -> Bool 
dividierbar a0 [] = False
dividierbar a0 (x:xs) = (rem a0 x == 0) || (dividierbar a0 xs)
