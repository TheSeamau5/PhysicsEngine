import Physics.Math.Point (..)
import Physics.Body (..)
import Physics.Collider (..)
import Keyboard

---------------------------------------------
-- List helper functions
swap1_2 : (a -> b -> c ) -> (b -> a -> c)
swap1_2 = flip

swap1_3 : (c -> b -> a -> d) -> (a -> b -> c -> d)
swap1_3 f x y z = f z y x

swap2_1 : (a -> b -> c) -> (b -> a -> c)
swap2_1 = swap1_2

swap2_3 : (a -> c -> b -> d) -> (a -> b -> c -> d)
swap2_3 f x y z = f x z y

swap3_1 : (c -> b -> a -> d) -> (a -> b -> c -> d)
swap3_1 = swap1_3

swap3_2 : (a -> c -> b -> d) -> (a -> b -> c -> d)
swap3_2 = swap2_3

toFront_2 : (a -> b -> c) -> (b -> a -> c)
toFront_2 = swap1_2

toFront_3 : (c -> a -> b -> d) -> (a -> b -> c -> d)
toFront_3 = swap2_3 swap1_3

toBack_2 : (a -> b -> c) -> (b -> a -> c)
toBack_2 = swap1_2

toBack_3 : (b -> c -> a -> d) -> (a -> b -> c -> d)
toBack_3 =  swap1_2 . swap2_3

toList : a -> [a]
toList = (flip (::)) []

takeNth : Int -> [a] -> a
takeNth n =  last . take n

removeNth : Int -> [a] -> [a]
removeNth n list = (take (n-1) list) ++ (drop n list)

applyNth : Int -> (a -> a -> b) -> [a] -> [b]
applyNth n f list = map ((f . takeNth n) list) (removeNth n list)

applyAll : (a -> a -> b) -> [a] -> [[b]]
applyAll f list = map ((toFront_3 applyNth) f list) [1..(length list)]

---------------------------------------------

type GameObject a b c d   = { collider  : Collider a b 
                            , body      : Body c d 
                            , color     : Color 
                            }
type Scene a b c d = [GameObject a b c d] 



gameObject : Collider a b -> Body c d -> Color -> GameObject a b c d
gameObject collider body color =
  { collider  = collider
  , body      = body 
  , color     = color
  } 

squareObject : Color -> Float -> Point a -> Float -> GameObject {} a a {}
squareObject color mass center size = 
  gameObject  (collider center size size)
              (dynamicBody mass center origin)
              color


drawGameObject : GameObject a b c d -> Form
drawGameObject gameObject = 
  move ( gameObject.collider.center.x , gameObject.collider.center.y )
       (filled gameObject.color ( rect gameObject.collider.width 
                                  gameObject.collider.height
                                )
       )

drawScene : Int -> Int -> Scene a b c d -> Element
drawScene width height = (collage width height) . map drawGameObject

hero : GameObject {} {} {} {}
hero = squareObject blue 50 (point 50 50) 50


render : Signal (Scene a b c d) -> Signal Element
render = lift (drawScene 400 400)

updateObject : GameObject {} {} {} {} -> Point {} -> GameObject {} {} {} {} 
updateObject obj p = 
  { obj | collider  <-  collider  (obj.body.position) 
                                  (obj.collider.width) 
                                  (obj.collider.height)
        , body      <-  applyForceNow p obj.body 
  }

updatePhysics : GameObject {} {} {} {} -> Signal (Point {}) -> Signal (Scene {} {} {} {})
updatePhysics obj p = lift toList (foldp (flip updateObject) obj p)

getInput : Signal (Point {})
getInput = foldp (<+>) origin (lift toFloatPoint Keyboard.arrows)

update : GameObject {} {} {} {} -> Signal Element
update obj = getInput |> updatePhysics obj |> render

main = update hero





-- update scene = getInput |> handleInput |> updateWorld |> updatePhysics |> render


{-

  type World a = { a | gravity : Point {} }
  type Scene a b c d e = { world : World a , gameObjects : [GameObject b c d e] }



  render : Scene a b c d e -> Signal Element
  render scene = -- draw all gameObjects in scene

  updatePhysics : Signal (Scene a b c d e) -> Signal (Scene a b c d e)
  updatePhysics scene = updateWorld scene |> updateObjects scene

  handleSignals : Scene a b c d e -> Signal f -> Signal (Scene a b c d e)
  handleSignals = -- update all necessary interactions 

  getSignals : Signal f
  getSignals = getInput |> getTime

  update : Scene a b c d e -> Signal Element
  update scene = getSignals |> handleSignals scene
                            |> updatePhysics scene
                            |> render scene




-}







--update : Scene a b c d -> Signal Element
--update scene = getInput |> updatePhysics scene |> render

--main = render (lift toList (foldp applyForceNow hero (foldp (<+>) origin (lift toFloatPoint Keyboard.arrows))))

--main = lift asText (foldp applyForceNow hero.body (foldp (<+>) origin (lift toFloatPoint Keyboard.arrows)))
--main = drawScene 400 400 [hero]




{-}

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
-}
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
