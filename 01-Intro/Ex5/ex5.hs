{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use min" #-}
sgn :: Int -> Int
absInt :: Int -> Int
min2Int :: (Int, Int) -> Int

main :: IO ()

sgn n = 
    if n < 0 then -1
    else if n == 0 then 0
    else 1

absInt n =
    if n >= 0 then n
    else -n

min2Int (n, m) =
    if n <= m then n
    else m

main = return ()