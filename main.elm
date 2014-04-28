import Physics.Math.Point (..)
import Physics.Body (..)

hero = dynamicBody 40 origin origin

main = asText (applyForce earthGravity hero)