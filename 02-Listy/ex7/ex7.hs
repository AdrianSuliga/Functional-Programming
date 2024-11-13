sum' :: (Num a) => [a] -> a
sum' xs = loop 0 xs
  where
    loop acc [] = acc
    loop acc (x : xs) = loop (x + acc) xs

prod' :: (Num a) => [a] -> a
prod' xs = loop 1 xs
  where
    loop acc [] = acc
    loop acc (x : xs) = loop (x * acc) xs

leng' :: (Num a) => [a] -> a
leng' xs = loop 0 xs
  where
    loop acc [] = acc
    loop acc (x : xs) = loop (1 + acc) xs

absAll :: (Num a) => [a] -> [a]
absAll xs = reverse (loop [] xs)
  where
    loop acc [] = acc
    loop acc (x : xs) = loop (abs x : acc) xs