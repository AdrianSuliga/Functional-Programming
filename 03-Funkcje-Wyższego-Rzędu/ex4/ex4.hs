funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

disp1Eqs :: (Double -> Double, Double -> Double)
disp1Eqs = (\t -> 4 * t^2 + 2 * t, \t -> 2 * t^2)