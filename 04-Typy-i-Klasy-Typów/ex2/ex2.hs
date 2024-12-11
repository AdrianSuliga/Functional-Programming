data CartInt2DVec = MkCartInt2DVec Int Int
data CartInt2DVec' a = MkCartInt2DVec' {x1::a, y1::a}
data CartInt3DVec a = MkCartInt3DVec {x::a, y::a, z::a}
data List a = EmptyL | Cons a (List a) deriving Show
data ThreeColors = Blue | White | Red 
type ActorName = String
leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "AS"
leadingActor White = "WS"
leadingActor Red = "RS"

head' :: List a -> a
head' EmptyL = error "head': the empty list has no head"
head' (Cons x xs) = x

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y