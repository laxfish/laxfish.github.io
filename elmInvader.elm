--Elm Invader Final Project Part 1, Bridger Fisher & Ian Sime
import Html exposing(program)
import Collage exposing (..)
import Keyboard exposing (..)
import AnimationFrame
import Time exposing (..)
import Window
import Color exposing(..)
import Element exposing (..)
import Text exposing (..)

type Msg = KeyMsg Key | Tick Time
type Key = KeyRelease Int | KeyPress Int

initialModel = { x = 0, vx = 0,
                 y = 0, vy = 0,
               groundAcceleration = 0}

main = program
 { init = (initialModel, Cmd.none),
   view = view,
   subscriptions = subscriptions,
   update = updateWithCommand }

subscriptions model =
    Sub.batch
    [ Keyboard.downs (KeyPress >> KeyMsg)
    , Keyboard.ups (KeyRelease >> KeyMsg),
    AnimationFrame.diffs Tick
    ]

update msg model =
    case msg of
      Tick time -> tick model
      KeyMsg k -> key k model

updateWithCommand msg model =
  (update msg model, Cmd.none)

horizontalVelocity = 10
shootVelocity = 10
gravityAcceleration = 0

type alias LineStyle =
    { color : Color
    , width : Float
    , cap : LineCap
    , join : LineJoin
    , dashing : List Int
    , dashOffset : Int
    }

view model =
    toHtml(
      collage 1000 500 [(
        (moveY -200 (moveX model.x (toForm (image 100 100 "Tank.png"))))),

        (moveY (model.y - 250) (moveX model.x (filled (black ) (circle 7)))),

         (moveY -250 (filled (black ) (rect 1000 20))),

         (moveY 200 (moveX -model.x (toForm (image 100 100 "Alien.png")))),

         ( toForm (centered (fromString ("Press left/right arrows to control tank and the up arrow to shoot"))))

        ]
        )


type alias Bullet =           --beginnig to set up the physics of a bullet
  { positionX : Int
  , positionY : Int
  , velocityX : Int
  , velocityY : Int
  }

key msg model =
    model
        |> case msg of
            KeyPress 38 -> shoot      --press up key to shoot

            KeyPress 37 -> accelerateTo -horizontalVelocity
            KeyRelease   37 -> stop (<)

            KeyPress 39 -> accelerateTo horizontalVelocity
            KeyRelease   39 -> stop (>)


            _ -> identity


accelerateTo velocity model =
    {model | groundAcceleration = velocity}

shoot model =
    if model.vy == 0 then
        {model | vy = shootVelocity}
    else
        model

stop compare model =
    if compare model.groundAcceleration 0 then
       {model | groundAcceleration = 0}
    else
        model

tick model =
    model
        |> traction
        |> motion
        |> gravity
        |> floor

traction model =
    if model.y == 0 then
       {model | vx = model.groundAcceleration}
    else
       model

motion model =
    {model | x = model.x + model.vx,
             y = model.y + model.vy}

floor model =
    if model.y < 0 then
       {model | y = 0, vy = 0}
    else
       model


gravity model =
    {model | vy = model.vy}
