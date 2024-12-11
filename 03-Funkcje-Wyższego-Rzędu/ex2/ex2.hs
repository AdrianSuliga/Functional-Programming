sums :: Num a => [a] -> a
sums [] = 0
sums (x:xs) = x + sums xs

sumSqr :: Num a => [a] -> a
sumSqr xs = sums [i*i | i <- xs]

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum2 :: Num a => [a] -> a 
sumSqr2 :: Num a => [a] -> a
sumCube :: Num a => [a] -> a
sumAbs :: Num a => [a] -> a
listLength :: Num a => [a] -> a

sum2 = sumWith (\x -> x)
sumSqr2 = sumWith (\x -> x^2)
sumCube = sumWith (\x -> x^3)
sumAbs = sumWith(\x -> abs(x))
listLength = sumWith(\x -> 1)

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 1
prodWith f (x:xs) = f x * prodWith f xs

prod = prodWith (\x -> x)
prodSqr = prodWith (\x -> x^2)
prodCube = prodWith (\x -> x^3)
prodAbs = prodWith (\x -> abs(x))