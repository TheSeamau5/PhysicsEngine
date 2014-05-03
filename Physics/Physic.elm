module Physics.Physic where

import Physics.Math.Point (..)
import Physics.Utilities.Basics (..)
import Physics.Utilities.List (..)
import Physics.Body (..)
import Physics.Collider (..)


type Physic = { collider : Collider
              , body : Body {} {}
              }



{-
type GameObject a b c d = { collider  : Collider a b
                          , body      : Body c d
                          , color     : Color 
                          , width     : Float
                          , height    : Float
                          }

gameObject : Collider a b -> Body c d -> Color -> GameObject a b c d 
gameObject collider body color width height  = 
  { collider  = collider 
  , body      = body 
  , color     = color 
  , width     = width 
  , height    = height  
  }

-- mostly for testing purposes
squareObject : BodyType -> Color -> Float -> Point a -> Float -> GameObject {} a a {}
squareObject bodyType color mass center size = 
  gameObject  (collider center size size)
              (body bodyType mass center origin)
              color
              size 
              size 

drawGameObject : GameObject a b c d -> Form 
drawGameObject gameObject = 
  move  ( gameObject.body.position.x , gameObject.body.position.y )
        ( filled gameObject.color (rect gameObject.width gameObject.height))

updateObject : GameObject a b c d -> Float -> GameObject a b c d 
updateObject obj timestep = 
  { obj | collider  <- collider  (obj.body.position)
                                 (obj.collider.width)
                                 (obj.collider.height)
        , body      <- updateBody obj.body timestep 
  }
-}