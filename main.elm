import Physics.Math.Point (..)
import Physics.Body (..)
import Physics.Collider (..)
import Physics.Physic (..)
import Keyboard


foo = circleCollider origin 50
bar = circleCollider (point -50 0) 50 

render : Collider -> Collider -> Element
render collider1 collider2 =
  let colliderColor = 
    case collide collider1 collider2 of
      True -> red 
      False -> green 
  in collage 400 400 
     [ drawCollider colliderColor collider1 
     , drawCollider colliderColor collider2]

getInput : Collider -> Signal Collider
getInput hero = 
  case hero of
    CircleCollider a -> lift2 circleCollider 
                              (foldp  (<+>) 
                                      a.center 
                                      (lift (scaleBy 10) 
                                            (lift toFloatPoint 
                                                  Keyboard.arrows))) 
                              (constant a.radius)
    _ -> constant hero

update : Collider -> Collider -> Signal Element
update hero test = getInput hero |> lift2 render (constant test)

main = update foo bar