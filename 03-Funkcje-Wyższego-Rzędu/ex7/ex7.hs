import Distribution.Simple.Program.HcPkg (list)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) | p x = x : filter' p xs
                 | otherwise = filter' p xs

onlyEven = filter' (\x -> x `mod` 2 == 0)  
onlyOdd = filter' (\x -> x `mod` 2 /= 0)