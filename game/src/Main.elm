module Main exposing (main)

import Playground exposing (..)

type alias Vector a = {x: a, y: a}
type alias CoordinatesType a = Vector a -> (a, a)
coordinates v = (v.x, v.y) 

type alias Body = { position: Vector Float, speed: Vector Float, bearing: Float, size: Float}

type alias State = List Body
initialState: State
initialState = [{ position = { x = 0.0, y = 0.0 }, speed = { x = 0.0, y = 0.0 }, bearing = 0.0, size = 20.0}]

tickBodyUpdate: Computer -> Body -> Body
tickBodyUpdate computer body =
  let
    dimPos d vd = d + vd
    newPos = {x = dimPos body.position.x body.speed.x, y = dimPos body.position.y body.speed.y}
    dimSpeed from to = (to - from)/10.0
    newSpeed = { x = dimSpeed newPos.x computer.mouse.x, y = dimSpeed newPos.y computer.mouse.y}
  in {body | speed = newSpeed, position = newPos}

renderBody: Body -> Shape
renderBody body =
  let
    {x, y} = body.position
    -- y = body.position.y
  in circle black (body.size) |> move x y

main = game view update initialState

view: Computer -> State -> List Shape
view _ state = List.map renderBody state

update: Computer -> List Body -> List Body
update computer state = List.map (tickBodyUpdate computer) state