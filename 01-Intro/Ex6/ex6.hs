absInt :: Int -> Int
sgn :: Int -> Int
min3Int :: (Int, Int, Int) -> Int
romanDigit :: Char -> String

main :: IO ()

absInt n 
    | n >= 0 = n
    | otherwise = -n

sgn n
    | n > 0 = 1
    | n == 0 = 0
    | n < 0 = -1

min3Int (x, y, z)
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | z <= x && z <= y = z

romanDigit c
    | c == '1' = "I"
    | c == '2' = "II"
    | c == '3' = "III"
    | c == '4' = "IV"
    | c == '5' = "V"
    | c == '6' = "VI"
    | c == '7' = "VII"
    | c == '8' = "VIII"
    | c == '9' = "IX"

main = return ()