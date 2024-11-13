{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
main :: IO ()
absInt :: Int -> Int
roots :: (Double, Double, Double) -> (Double, Double)

absInt n =
    case n >= 0 of
        True -> n
        _ -> -n

--roots (a, b, c) = ((-b - d) / e, (-b + d)/e)
--    where d = sqrt(b*b-4*a*c)
--          e = 2 * a

roots (a, b, c) = 
    let d = sqrt(b*b - 4*a*c)
        e = 2 * a
    in ((-b-d)/e, (-b+d)/e)

main = return ()