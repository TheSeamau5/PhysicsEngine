module Physics.Collider where

import Physics.Math.Point (..)
import Physics.Utilities.List (..)
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

{-
centroid : PolygonShape -> Point {}
centroid poly = 
  case length poly of
    0 -> origin
    1 -> head poly
    2 -> center (poly !! 1) (poly !! 2)
    n -> map (\i l -> ((.x) (l || i)) * ((.y) (l || (i + 1))) - ((.x) (l || (i + 1))) * ((.y) (l || i)) )
    n ->
      let cxhelper l i = 
        let ith   = l !! i
            ithp1 = l !! (i + 1)
        in fmap2 (+) (fmap (.x) ith) (fmap (.x) ithp1)
          cyhelper l i = 
        let ith   = l !! i
            ithp1 = l !! (i + 1)
        in fmap2 (+) (fmap (.y) ith) (fmap (.y) ithp1)
          helperCrossDot l i = 
        let ith   = l !! i
            ithp1 = l !! (i + 1)
        in fmap2 (-) 
                 (fmap2 (*) ( fmap (.x) ith   ) ( fmap (.y) ithp1)) 
                 (fmap2 (*) ( fmap (.x) ithp1 ) ( fmap (.y) ith  ))
          A = 0.5 * sum (map (helperCrossDot poly) [1..((length poly) - 1)])
        
            
-}
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


drawCollider color collider = 
  case collider of
    AABBCollider a   -> move (a.center.x , a.center.y)
                             (outlined (solid color) (rect a.width a.height))
    CircleCollider a -> move (a.center.x, a.center.y)
                             (outlined (solid color) (circle a.radius))
    EdgeCollider a   -> (outlined (solid color) (path [ (a.vertex1.x , a.vertex1.x)
                                                    , (a.vertex2.x , a.vertex2.y) 
                                                    ]
                                              )
                        )
    _ -> filled color (rect 40 40)
    --PolygonCollider a -> 



------------------------------------------------------------

{-

type Collider a b = { a | center  : Point b 
                        , width   : Float
                        , height  : Float
                    }

collider : Point a -> Float -> Float -> Collider {} a
collider center width height = 
    { center  = center
    , width   = width
    , height  = height 
    }  

collide : Collider a b -> Collider c d -> Bool
collide collider1 collider2 =
    let halfWidth1  = collider1.width  / 2
        halfWidth2  = collider2.width  / 2
        halfHeight1 = collider1.height / 2
        halfHeight2 = collider2.height / 2 
        left1       = collider1.center.x - halfWidth1
        left2       = collider2.center.x - halfWidth2
        right1      = collider1.center.x + halfWidth1
        right2      = collider2.center.x + halfWidth2
        top1        = collider1.center.y + halfHeight1
        top2        = collider2.center.y + halfHeight2
        bottom1     = collider1.center.y - halfHeight1
        bottom2     = collider2.center.y - halfHeight2
    in not ( (left2 > right1) || (right2 < left1) || (top2 > bottom1) || (bottom2 < top1) )

-}







