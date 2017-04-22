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

--Set up the message for key presses and time
type Msg = KeyMsg Key | Tick Time
type Key = KeyRelease Int | KeyPress Int

--initialize the model coordinates
initialModel = { x = 0, vx = 0,
                 y = 0, vy = 0,
               groundAcceleration = 0}
--initialize the program
main = program
 { init = (initialModel, Cmd.none),
   view = view,
   subscriptions = subscriptions,
   update = updateWithCommand }

--set up the subscriptions for the keypresses and time
subscriptions model =
    Sub.batch
    [ Keyboard.downs (KeyPress >> KeyMsg)
    , Keyboard.ups (KeyRelease >> KeyMsg),
    AnimationFrame.diffs Tick
    ]

-- update the model after time and/or keypresses
update msg model =
    case msg of
      Tick time -> tick model
      KeyMsg k -> key k model

--update the model after time and/or keypresses helper
updateWithCommand msg model =
  (update msg model, Cmd.none)

--initialize the horizontal velocity of tank, shot velocity of bullet, and gravity
horizontalVelocity = 10
shootVelocity = 10
gravityAcceleration = 0

--initialize the line style
type alias LineStyle =
    { color : Color
    , width : Float
    , cap : LineCap
    , join : LineJoin
    , dashing : List Int
    , dashOffset : Int
    }

--set up the view of the model
view model =
    toHtml(
      collage 1000 500 [(
        (moveY -200 (moveX model.x (toForm (image 100 100 "Tank.png"))))),
        --tank
        (moveY (model.y - 250) (moveX model.x (filled (black ) (circle 7)))),
        -- bullet moves with tank and fires, issue right now is that after it shoots it breaks
         (moveY -250 (filled (black ) (rect 1000 20))),
        -- line is created for the floor
         (moveY 200 (moveX -model.x (toForm (image 100 100 "Alien.png")))),
        -- alien is created and only moves opposite the tank for now
         ( toForm (centered (fromString ("Press left/right arrows to control tank and the up arrow to shoot"))))
        -- adds text instructions for now
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

            KeyPress 37 -> accelerateTo -horizontalVelocity   --movement left
            KeyRelease   37 -> stop (<)

            KeyPress 39 -> accelerateTo horizontalVelocity    --movement right
            KeyRelease   39 -> stop (>)

            _ -> identity


--sets the acceleration once left/right arrow is pressed
accelerateTo velocity model =
    {model | groundAcceleration = velocity}

--sets the shot once the up arrow is pressed
shoot model =
    if model.vy == 0 then
        {model | vy = shootVelocity}
    else
        model

--sets the deceleration of the tank once the left/right arrow is let go
stop compare model =
    if compare model.groundAcceleration 0 then
       {model | groundAcceleration = 0}
    else
        model

--sets the time
tick model =
    model
        |> traction
        |> motion
        |> gravity
        |> floor

--sets the friction of the ground/tank
traction model =
    if model.y == 0 then
       {model | vx = model.groundAcceleration}
    else
       model

--sets the coordinates for change in location
motion model =
    {model | x = model.x + model.vx,
             y = model.y + model.vy}

--sets the floor and keeps tank above
floor model =
    if model.y < 0 then
       {model | y = 0, vy = 0}
    else
       model
--sets the gravity of the view, right now it is zero but might be changed to
--accompany the falling of the aliens
gravity model =
    {model | vy = model.vy}
