primes :: [Int]
primes = eratoSieve [2 ..]
  where
    eratoSieve :: [Int] -> [Int]
    eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

primesCount :: Int -> [Int]
primesCount n = take n (eratoSieve [2 ..])
  where
    eratoSieve :: [Int] -> [Int]
    eratoSieve [] = []
    eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]
