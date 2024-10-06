sqr :: Double -> Double
vec2DLen :: (Double, Double) -> Double
vec3DLen :: (Double, Double, Double) -> Double
swap :: (Int, Char) -> (Char, Int)
threeEqual :: (Int, Int, Int) -> Bool
area :: (Double, Double, Double) -> Double

main :: IO ()

sqr x = x * x
vec2DLen (x, y) = sqrt (x^2 + y^2)
vec3DLen (x, y, z) = sqrt (x^2 + y^2 + z^2)
swap (x, y) = (y, x)
threeEqual (x, y, z) = (x == y) && (y == z)
area (a, b, c) = sqrt((a + b + c) * (a + b - c) * (a + c - b) * (b + c - a) / 16) 

main = do
    let x = area(1, 1, 1)
    print x