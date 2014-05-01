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

---------------------------------------------------

type Centered a b  = { a | center : Point b }

----------------------------------------------------

type AABBShape     = Centered { width : Float 
                              , height : Float } {}

type CircleShape   = Centered { radius : Float } {}

type EdgeShape     = { vertex1 : Point {} 
                     , vertex2 : Point {}
                     }

type PolygonShape  = [Point {} ]

-----------------------------------------------------

data Collider = AABBCollider AABBShape
              | CircleCollider CircleShape
              | EdgeCollider EdgeShape
              | PolygonCollider PolygonShape


aabbCollider : Point {} -> Float -> Float -> Collider
aabbCollider center width height = 
  AABBCollider { center = center 
               , width  = width
               , height = height 
               }

circleCollider : Point {} -> Float -> Collider
circleCollider center radius =
  CircleCollider { center = center 
                 , radius = radius
                 }
   
edgeCollider : Point {} -> Point {} -> Collider
edgeCollider vertex1 vertex2 =
  EdgeCollider { vertex1 = vertex1
               , vertex2 = vertex2
               }

polygonCollider : [Point {}] -> Collider
polygonCollider points = PolygonCollider points

------------------------------------------------------



aabbVSaabb : AABBShape -> AABBShape -> Bool
aabbVSaabb aabb1 aabb2 = 
  let halfWidth1  = aabb1.width  / 2
      halfHeight1 = aabb1.height / 2
      left1       = aabb1.center.x - halfWidth1
      right1      = aabb1.center.x + halfWidth1
      top1        = aabb1.center.y + halfHeight1
      bottom1     = aabb1.center.y - halfHeight1
      halfWidth2  = aabb2.width  / 2
      halfHeight2 = aabb2.height / 2
      left2       = aabb2.center.x - halfWidth2
      right2      = aabb2.center.x + halfWidth2
      top2        = aabb2.center.y + halfHeight2
      bottom2     = aabb2.center.y - halfHeight2
  in not ( (left2 > right1) || (right2 < left1) || (top2 > bottom1) || (bottom2 < top1) )

aabbVScircle : AABBShape -> CircleShape -> Bool
aabbVScircle aabb circle = 
  let pointInRectangle p r     =  let halfWidth  = r.width  / 2
                                      halfHeight = r.height / 2
                                      left       = r.center.x - halfWidth
                                      right      = r.center.x + halfWidth
                                      top        = r.center.y + halfHeight
                                      bottom     = r.center.y - halfHeight
                                  in  ((p.x < right) && (p.x > left) && (p.y < top) && (p.y > bottom))
      closestPointToCircle r c =  let halfWidth  = r.width  / 2
                                      halfHeight = r.height / 2
                                      left       = r.center.x - halfWidth
                                      right      = r.center.x + halfWidth
                                      top        = r.center.y + halfHeight
                                      bottom     = r.center.y - halfHeight
                                      centerX    = c.center.x
                                      centerY    = c.center.y
                                  in point (max (min centerX right) left) (max (min centerY bottom) top)
  in (pointInRectangle (circle.center) aabb) ||
     ((distance (closestPointToCircle aabb circle) circle.center) < circle.radius)

aabbVSedge : AABBShape -> EdgeShape -> Bool
aabbVSedge aabb edge = False 

aabbVSpolygon : AABBShape -> PolygonShape -> Bool
aabbVSpolygon aabb polygon = False 


circleVSaabb : CircleShape -> AABBShape -> Bool
circleVSaabb circle aabb = aabbVScircle aabb circle

circleVScircle : CircleShape -> CircleShape -> Bool
circleVScircle circle1 circle2 = 
  (circle1.radius + circle2.radius) < distance circle1.center circle2.center

circleVSedge : CircleShape -> EdgeShape -> Bool
circleVSedge circle edge = False

circleVSpolygon : CircleShape -> PolygonShape -> Bool
circleVSpolygon circle polygon = False


edgeVSaabb : EdgeShape -> AABBShape -> Bool
edgeVSaabb edge aabb = aabbVSedge aabb edge

edgeVScircle : EdgeShape -> CircleShape -> Bool
edgeVScircle edge circle = circleVSedge circle edge

edgeVSedge : EdgeShape -> EdgeShape -> Bool
edgeVSedge edge1 edge2 = False

edgeVSpolygon : EdgeShape -> PolygonShape -> Bool
edgeVSpolygon edge polygon = False


polygonVSaabb : PolygonShape -> AABBShape -> Bool
polygonVSaabb polygon aabb = aabbVSpolygon aabb polygon

polygonVScircle : PolygonShape -> CircleShape -> Bool
polygonVScircle polygon circle = circleVSpolygon circle polygon

polygonVSedge : PolygonShape -> EdgeShape -> Bool
polygonVSedge polygon edge = edgeVSpolygon edge polygon

polygonVSpolygon : PolygonShape -> PolygonShape -> Bool
polygonVSpolygon polygon1 polygon2 = False


collide : Collider -> Collider -> Bool
collide collider1 collider2 =
  case (collider1, collider2) of
    ( AABBCollider a     , AABBCollider b    )  ->  aabbVSaabb a b
    ( AABBCollider a     , CircleCollider b  )  ->  aabbVScircle a b
    ( AABBCollider a     , EdgeCollider b    )  ->  aabbVSedge a b
    ( AABBCollider a     , PolygonCollider b )  ->  aabbVSpolygon a b
    ( CircleCollider a   , AABBCollider b    )  ->  circleVSaabb a b
    ( CircleCollider a   , CircleCollider b  )  ->  circleVScircle a b
    ( CircleCollider a   , EdgeCollider b    )  ->  circleVSedge a b
    ( CircleCollider a   , PolygonCollider b )  ->  circleVSpolygon a b
    ( EdgeCollider a     , AABBCollider b    )  ->  edgeVSaabb a b
    ( EdgeCollider a     , CircleCollider b  )  ->  edgeVScircle a b
    ( EdgeCollider a     , EdgeCollider b    )  ->  edgeVSedge a b
    ( EdgeCollider a     , PolygonCollider b )  ->  edgeVSpolygon a b
    ( PolygonCollider a  , AABBCollider b    )  ->  polygonVSaabb a b
    ( PolygonCollider a  , CircleCollider b  )  ->  polygonVScircle a b
    ( PolygonCollider a  , EdgeCollider b    )  ->  polygonVSedge a b
    ( PolygonCollider a  , PolygonCollider b )  ->  polygonVSpolygon a b
    _ -> False


------------------------------------------------------------------------------


drawCollider collider = 
  case collider of
    AABBCollider a   -> move (collider.center.x , collider.center.y)
                             (outlined (solid red) (rect collider.width collider.height))
    CircleCollider a -> move (collider.center.x, collider.center.y)
                             (outlined (solid red) (circle collider.radius))
    EdgeCollider a   -> 





------------------------------------------------------------


foo = aabbCollider origin 20 30
bar = circleCollider origin 40
baz = edgeCollider origin (point 10 20)
fuz = polygonCollider [ point 0 -1 
                      , point 1 0
                      , point 0 1
                      , point -1 0]
                    

test collider = 
  case collider of
    AABBCollider a -> "I am an axis aligned bounding box"
    CircleCollider a -> "I am a circle"
    EdgeCollider a -> "I am an Edge"
    PolygonCollider a -> "I am a Polygon"
    

test2 collider1 collider2 =
  case (collider1, collider2) of
    (AABBCollider a, AABBCollider b) -> "2 boxes"
    (CircleCollider a , CircleCollider b) -> "2 circles"
    (EdgeCollider a , CircleCollider b) -> "edge and circle"
    _ -> "2 colliders"



main = asText (test2 baz bar)