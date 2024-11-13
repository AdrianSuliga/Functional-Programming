import Language.Haskell.TH (Lit (IntegerL))

fib :: (Num a, Eq a) => a -> a
fib n =
  if n == 0 || n == 1
    then n
    else fib (n - 1) + fib (n - 2)

fib2 :: Int -> [Integer]
fib2 n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Integer]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

prod' :: (Num a) => [a] -> a
prod' [] = 1
prod' (x : xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x : xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [x] = x
or' (x : xs) = x || or' xs

and' :: [Bool] -> Bool
and' [x] = x
and' (x : xs) = x && and' xs

doubleAll :: (Num t) => [t] -> [t]
doubleAll [] = []
doubleAll (x : xs) = [2 * x] ++ doubleAll xs

squareAll :: (Num t) => [t] -> [t]
squareAll [] = []
squareAll (x : xs) = [x ^ 2] ++ squareAll xs

absAll :: (Num t) => [t] -> [t]
absAll [] = []
absAll (x : xs) = abs x : absAll xs