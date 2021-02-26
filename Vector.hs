module Vector
( Vector(..)
, Vector3(..)
, vecAdd
, vecMul
, vecDot
, vec3Add
, vec3Mul
, vec3Dot
) where


data Vector a = Vector a a deriving (Show, Eq)

data Vector3 a = Vector3 a a a deriving (Show, Eq)

vecAdd :: (Num a) => Vector a -> Vector a -> Vector a
vecAdd (Vector u v) (Vector x y) = Vector (u+x) (v+y)

vecMul :: (Num a) => Vector a -> a -> Vector a
vecMul (Vector u v) m = Vector (u*m) (v*m)

vecDot :: (Num a) => Vector a -> Vector a -> a
vecDot (Vector u v) (Vector x y) = u*x + v*y

vec3Add :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
vec3Add (Vector3 u v w) (Vector3 x y z) = Vector3 (u+x) (v+y) (w+z)

vec3Mul :: (Num a) => Vector3 a -> a -> Vector3 a
vec3Mul (Vector3 u v w) m = Vector3 (u*m) (v*m) (w*m)

vec3Dot :: (Num a) => Vector3 a -> Vector3 a -> a
vec3Dot (Vector3 u v w) (Vector3 x y z) = u*x + v*y + w*z

