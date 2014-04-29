module Physics.Body where 

import Physics.Math.Point (..)

-- There are two types of bodies, Static and Dynamic bodies
data BodyType = Static | Dynamic

-- Positioned is an extensible type with a "position" field 
type Positioned a b = { a | position  : Point b  }

-- Moving is an extensible type with a "velocity" field
type Moving a b     = { a | velocity  : Point b  }

-- Massive is an extensible type with a "mass" field
type Massive a      = { a | mass      : Float    }

-- BodyTyped is an extensible type with a "bodyType" field
type BodyTyped a    = { a | bodyType  : BodyType }

-- A body is any record with mass, position, velocity, and a BodyType
type Body p m = Massive (BodyTyped ( Positioned ( Moving {} m ) p))

-- basic constructor for Body Type
body : BodyType -> Float -> Point a -> Point b -> Body a b
body bodyType mass initialPoint initialVelocity = 
    { position  = initialPoint 
    , velocity  = initialVelocity
    , mass      = mass 
    , bodyType  = bodyType 
    }
-- basic constructor for a Static Body
staticBody  : Float -> Point a -> Point b -> Body a b
staticBody  = body Static

-- basic constructor for a Dynamic Body
dynamicBody : Float -> Point a -> Point b -> Body a b
dynamicBody = body Dynamic

-- Constructor to create a force. It is an alias for point.
force : Float -> Float -> Point {}
force = point

{-  Constructor to create a gravitational force. 
    Creates a force in the vertical direction 
    negative to the given magnitude -}
gravity : Float -> Point {}
gravity magnitude = force 0 (-magnitude)

{-  Constructor to create a gravitational force
    equal to that of Earth -}
earthGravity : Point {}
earthGravity    = gravity 9.81

{-  Constructor to create a gravitational force
    equal to that of Mercury -}
mercuryGravity : Point {}
mercuryGravity  = gravity 3.70

{-  Constructor to create a gravitational force
    equal to that of Venus -}
venusGravity : Point {}
venusGravity    = gravity 8.87

{-  Constructor to create a gravitational force
    equal to that of Mars -}
marsGravity : Point {}
marsGravity     = gravity 3.71

{-  Constructor to create a gravitational force
    equal to that of Jupiter -}
jupiterGravity : Point {}
jupiterGravity  = gravity 23.12

{-  Constructor to create a gravitational force
    equal to that of Saturn -}
saturnGravity : Point {}
saturnGravity   = gravity 10.44

{-  Constructor to create a gravitational force
    equal to that of Uranus -}
uranusGravity : Point {}
uranusGravity   = gravity 8.69

{-  Constructor to create a gravitational force
    equal to that of Neptune -}
neptuneGravity : Point {}
neptuneGravity  = gravity 11.00

{-  Constructor to create a gravitational force
    equal to that of Pluto -}
plutoGravity : Point {}
plutoGravity    = gravity 0.60

{-  Constructor to create a gravitational force
    equal to that of The Sun -}
sunGravity : Point {}
sunGravity      = gravity 274.0

{-  Constructor to create a gravitational force
    equal to that of The Moon -}
moonGravity : Point {}
moonGravity     = gravity 1.62


{-  Function to apply a force to a body 
   ( This function assumes that the timestep == 1)
   ( This greatly simplifies the calculations )
   ( As it means that acceleration == velocity )
   ( This means that newVelocity = oldVelocity + force / mass)
   ( Note : This function only updates the velocity, not the position)-}
applyForce  : Point a -> Body b c -> Body b c 
applyForce force body = 
  if  | (body.bodyType == Dynamic) -> 
            { body | velocity <- ( scaleBy ( 1 / body.mass ) force ) <+> body.velocity }
      | otherwise -> body

{-  Function to update a body's position by its velocity
  ( This function must be called in order for a body to move)
  ( The applyForce function does not change a body's position)
  ( Only the updateBody function does)
  ( Again, the timeStep is assumed to be 1)
  ( This means that newPosition = oldPosition + velocity)
  -}
updateBody : Body a b -> Body a b 
updateBody body =
  if  | (body.bodyType == Dynamic) -> 
            { body | position <- body.velocity <+> body.position }
      | otherwise -> body 

{-  Quick and easy Function to apply a force and update the body.
    This Function applies the force to the body and affects
    the position of the body.-}
applyForceNow : Point a -> Body b c -> Body b c 
applyForceNow force body = applyForce force body |> updateBody 