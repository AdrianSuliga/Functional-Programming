sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 -> \x -> x^4
    5 -> intFunc
    _ -> const n
    where
        intFunc x = x^5

silnia :: Int -> Double
silnia 0 = 1.0
silnia n = fromIntegral n * silnia (n - 1)

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo 0 = (\x -> 1.0)
expApproxUpTo n = (\x -> (x^n / silnia n + expApproxUpTo (n - 1) x))

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x + h) - f x) / h

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = \x -> (f (x + h) - f (x - h)) / 2 * h