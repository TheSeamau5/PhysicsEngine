import Physics.Math.Point (..)
import Physics.Body (..)
import Physics.Collider (..)
import Keyboard

---------------------------------------------
-- List helper functions

applyNth f list n = map (f (takeNth n [1..(length list)])) (removeNth n list)

applyAll f list = map (tryN f list) list
---------------------------------------------

type GameObject a b c d = { collider : Collider a b , body : Body c d , color : Color }
type Scene a b c d = [GameObject a b c d]


gameObject : Collider a b -> Body c d -> Color -> GameObject a b c d
gameObject collider body color =
    { collider  = collider 
    , body      = body 
    , color     = color
    }

drawGameObject : GameObject a b c d -> Form
drawGameObject gameObject = 
  move ( gameObject.collider.center.x , gameObject.collider.center.y )
       (filled gameObject.color ( rect gameObject.collider.width 
                                  gameObject.collider.height
                                )
       )

tolist : a -> [a]
tolist = (flip (::)) []

makeScene = collage 400 400 

drawScene : Scene a b c d -> Element
drawScene = makeScene . map drawGameObject

squareObject : Color -> Float -> Point a -> Float -> Float -> GameObject {} a a {}
squareObject color mass center width height = 
    gameObject  (collider center width height) 
                (dynamicBody mass center origin)
                color

hero = squareObject green 30 origin 50 50
evil = squareObject green 30 (point 100 0) 50 50

getInput = lift (scaleBy 100) (lift toFloatPoint Keyboard.arrows)

changeColor obj bool = 
    if  | bool == True -> { obj | color = Blue }
        | otherwise -> obj

checkCollisions scene = collide (head scene) (last scene)

updatePhysics scene =  checkCollisions scene |> changeColor (head scene) , tail scene

update scene = getInput |> updatePhysics scene |> render

main = [hero , evil] |> drawScene

{-
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

----------------------------------------------------------- 
-- TEST CODE

hero : Body {} {}
hero = dynamicBody 50 origin origin

main : Signal Element
main = update hero
-}
