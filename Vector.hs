module Vector
( Vector(..)
, Vector3(..)
, vecAdd
, vecSub
, vecMul
, vecCmp
, vecNeg
, vecLength
, vecNormalize
, vecDot
, vec3Add
, vec3Sub
, vec3Mul
, vec3Cmp
, vec3Neg
, vec3Length
, vec3Normalize
, vec3Dot
, vec3Cross
) where

-- A 2D vector
data Vector a = Vector a a deriving (Show, Eq)

-- A 3D vector
data Vector3 a = Vector3 a a a deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Vector functions
vecAdd :: (Num a) => Vector a -> Vector a -> Vector a
vecAdd (Vector x y) (Vector u v) = Vector (x+u) (y+v)

vecSub :: (Num a) => Vector a -> Vector a -> Vector a
vecSub (Vector x y) (Vector u v) = Vector (x-u) (y-v)

vecMul :: (Num a) => Vector a -> a -> Vector a
vecMul (Vector x y) m = Vector (x*m) (y*m)

vecCmp :: (Ord a) => Vector a -> Vector a -> Ordering
vecCmp (Vector x y) (Vector u v)
    | x < u = LT 
    | x > u = GT
    | y < v = LT
    | y > v = GT
    | otherwise = EQ

vecNeg :: (Num a) => Vector a -> Vector a
vecNeg (Vector x y) = Vector (-x) (-y)

vecLength :: (Floating a) => Vector a -> a
vecLength (Vector x y) = sqrt $ x^2 + y^2

vecNormalize :: (Floating a) => Vector a -> Vector a
vecNormalize v@(Vector x y) = Vector (x / len) (y / len)
    where len = vecLength v

vecDot :: (Num a) => Vector a -> Vector a -> a
vecDot (Vector x y) (Vector u v) = x*u + y*v

--------------------------------------------------------------------------------
-- Vector3 functions
vec3Add :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
vec3Add (Vector3 x y z) (Vector3 u v w) = Vector3 (u+x) (v+y) (w+z)

vec3Sub :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
vec3Sub (Vector3 x y z) (Vector3 u v w) = Vector3 (x-u) (y-v) (z-w)

vec3Mul :: (Num a) => Vector3 a -> a -> Vector3 a
vec3Mul (Vector3 x y z) m = Vector3 (x*m) (y*m) (z*m)

vec3Cmp :: (Ord a) => Vector3 a -> Vector3 a -> Ordering
vec3Cmp (Vector3 x y z) (Vector3 u v w)
    | x < u = LT
    | x > u = GT
    | y < v = LT
    | y > v = GT
    | z < w = LT
    | z > w = GT
    | otherwise = EQ

vec3Neg :: (Num a) => Vector3 a -> Vector3 a
vec3Neg (Vector3 x y z) = Vector3 (-x) (-y) (-z)

vec3Length :: (Floating a) => Vector3 a -> a
vec3Length (Vector3 x y z) = sqrt (x^2 + y^2 + z^2)

vec3Normalize :: (Floating a) => Vector3 a -> Vector3 a
vec3Normalize v@(Vector3 x y z) = Vector3 (x / len) (y / len) (z / len)
    where len = vec3Length v

vec3Dot :: (Num a) => Vector3 a -> Vector3 a -> a
vec3Dot (Vector3 x y z) (Vector3 u v w) = x*u + y*v + z*w

vec3Cross :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
vec3Cross (Vector3 x y z) (Vector3 u v w) = Vector3 (y*w - z*v) (z*u - x*w) (x*v - y*u)
