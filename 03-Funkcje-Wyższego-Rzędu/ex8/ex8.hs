map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElems = map' (\x -> 2 * x)
sqrtElems = map' (\x -> x^2)
