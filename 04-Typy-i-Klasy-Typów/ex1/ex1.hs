type CartesianCoord' a = (a, a)
type PolarCoord' a = (a, a)

newtype CartesianCoord'' a = MkCartesianCoord (a, a)
newtype PolarCoord'' a = MkPolarCoord (a, a)

polarToCartesian :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian (MkPolarCoord (r, phi)) = MkCartesianCoord (r * cos phi, r * sin phi)