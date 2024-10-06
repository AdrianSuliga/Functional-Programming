not' :: Bool -> Bool
or' :: (Bool, Bool) -> Bool
isItTheAnswer :: String -> Bool
main :: IO ()

not' True = False
not' False = True

or' (True, True) = True
or' (True, False) = True
or' (False, True) = True
or' (False, False) = False

isItTheAnswer "Love" = True
isItTheAnswer _ = False

main = return ()