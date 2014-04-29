module Physics.Collider where

import Physics.Math.Point (..)

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









