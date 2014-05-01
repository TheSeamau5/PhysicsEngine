module Physics.Math.Point where

-- 2D Extensible Point Type
type Point a = { a | x : Float , y : Float }

-- Basic constructor for point type
point : Float -> Float -> Point {}
point x y = { x = x , y = y }

-- Convert a record with x and y fields set to Int
-- to a similar record with fields set to Float 
toFloatPoint : { a | x : Int , y : Int } -> Point a
toFloatPoint p = { p | x <- toFloat p.x , y <- toFloat p.y }

-- The Point { x = 0 , y = 0 }
origin : Point {}
origin = point 0 0

-- The Point { x = 1 , y = 0 }
xUnit : Point {}
xUnit = point 1 0

-- The Point { x = 0 , y = 1 }
yUnit : Point {}
yUnit = point 0 1

-- Function to add two points
(<+>) : Point a -> Point b -> Point b
p <+> q = { q | x <- p.x + q.x , y <- p.y + q.y }

-- Function to subtract two points
(<->) : Point a -> Point b -> Point b
p <-> q = { q | x <- p.x - q.y , y <- p.y - q.y }

-- Function to multiply two points
(<*>) : Point a -> Point b -> Point b
p <*> q = { q | x <- p.x * q.y , y <- p.y * q.y }

-- Function to divide two points
(</>) : Point a -> Point b -> Point b
p </> q = { q | x <- p.x / q.y , y <- p.y / q.y }

-- Function to calculate the dot product of two points
(<.>) : Point a -> Point b -> Float
p <.> q = p.x * q.x + p.y * q.y

-- p-norm of a Point
norm : number -> Point a -> number
norm n v = ((v.x ^ n) + (v.y ^ n)) ^ (1 / n)

-- manhattan / taxicab norm
manhattanNorm : Point a -> number
manhattanNorm v = abs(v.x) + abs(v.y)

-- magnitude of a vector
magnitude : Point a -> number
magnitude = norm 2

-- scalar multiplication
scaleBy : number -> Point a -> Point a
scaleBy n v = { v | x <- v.x * n , y <- v.y * n }

-- distance between two points
distance : Point a -> Point b -> number
distance p q = magnitude (p <-> q)

-- manhattan / taxicab distance between two points
manhattanDistance : Point a -> Point b -> number
manhattanDistance p q = manhattanNorm (p <-> q)