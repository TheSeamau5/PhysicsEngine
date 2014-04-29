import Physics.Math.Point (..)
import Physics.Body (..)
import Keyboard

hero = dynamicBody 50 origin origin


drawBody : Body a b -> Element
drawBody body = collage 400 400 
                [move (body.position.x , body.position.y) (filled red (circle 50))]  

render : Signal (Body a b) -> Signal Element
render body = lift drawBody body


getInput : Signal (Point {})
getInput = lift (scaleBy 100) (lift toFloatPoint Keyboard.arrows)


updatePhysics : Body a b -> Signal (Point c) -> Signal (Body a b)
updatePhysics = foldp applyForceNow 

update : Body a b -> Signal Element
update body = getInput |> updatePhysics body |> render

main : Signal Element
main = update hero
