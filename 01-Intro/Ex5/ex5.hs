{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Language.Haskell.TH (fromE)
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use min" #-}
sgn :: Int -> Int
absInt :: Int -> Int
min2Int :: (Int, Int) -> Int
min3Int :: (Int, Int, Int) -> Int
toUpper :: Char -> Char
toLower :: Char -> Char
isDigit :: Char -> Bool
charToNum :: Char -> Int

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

min3Int (x, y, z) = min2Int(x, min2Int(y, z))

toUpper c = toEnum (fromEnum c - 32)

toLower c = toEnum (fromEnum c + 32)

isDigit c = 48 <= fromEnum c && fromEnum c <= 57 

charToNum = fromEnum

main = return ()