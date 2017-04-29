--Elm Invader Final Project Part 2, Bridger Fisher & Ian Sime
import Html exposing(program)
import Collage exposing (..)
import Keyboard exposing (..)
import AnimationFrame
import Time exposing (..)
import Window
import Color exposing(..)
import Element exposing (..)
import Text exposing (..)
import Random
import Collision2D exposing (..)

--initialize the model coordinates
initialModel = { x = 0,
                 y = 0,
               tank = initialTank,
               alien = initialAlien}
               {--bullet = [initialBullet]--}

main = program
    { init = (initialModel, Cmd.none),
      view = view,
      subscriptions = subscriptions,
      update = updateWithCommand
    }

--set up the subscriptions for the keypresses and time
subscriptions model =
    Sub.batch
    [ Keyboard.downs (KeyPress >> KeyMsg)
    , Keyboard.ups (KeyRelease >> KeyMsg),
    AnimationFrame.diffs Tick
    ]

type Msg = KeyMsg Key | Tick Time
type Key = KeyRelease Int | KeyPress Int

updateWithCommand msg model =
  (update msg model, Cmd.none)

-- update the model after time and/or keypresses
update msg model =
    case msg of
      Tick time -> tick model
      KeyMsg k -> key k model

--initialize the horizontal velocity of tank, shot velocity of bullet, and gravity
horizontalVelocity = 10
shotVelocity = 10
gravityAcceleration = -0.010
groundAcceleration = 0.01

type alias Tank = {
  x : Int,
  y : Int,
  vx : Int,
  vy : Int,
  groundAcceleration : Float,
  health : Int}

initialTank = {
  x = 0,
  y = -200,
  vx = 0,
  vy = 0,
  groundAcceleration = 0.0,
  health = 10}

tankMovement model =
  let tank1 = model.tank in
      { model | tank = {tank1 | x = model.tank.x + model.tank.vx, y = model.tank.y + model.tank.vy}}

type alias Alien = {
    y : Int,
    x : Int,
    vy : Int,
    alive : Bool,
    hitGround : Bool
}

initialAlien = {
  y = 200,
  x = 0,
  vy = gravityAcceleration,
  alive = True,
  hitGround = False}

alienMovement model =
  let alien1 = model.alien in
      { model | alien = {alien1 | y = model.alien.y - model.alien.vy}}

type alias Bullet =           --beginnig to set up the physics of a bullet
  { x : Float
  , y : Float
  , vx : Int
  , vy : Int
  , hit : Bool
  }

initialBullet tank model = {
  x = model.tank.x,
  y =  model.tank.y,
  vx = 0,
  vy = shotVelocity,
  hit = False}

--sets the shot once the up arrow is pressed
bulletMovement model =
  let bullet = model.bullet in
      { model | bullet = {bullet | y = model.bullet.y  model.bullet.vy}}

bulletVelocity shotVelocity model =
  let bullet = model.bullet in
  {model | bullet = {bullet | vy = model.bullet.vy + shotVelocity}}

spawnBullet bulletList model = {
  x = 0,
  y = 0,
  vx = 0,
  vy = shotVelocity,
  hit = False} {--:: bulletList--}

key msg model =
  model
    |> case msg of

       --up arrow
        --KeyPress 38 -> initialBullet

        --left arrow
        KeyPress 37 -> accelerateTo -horizontalVelocity
        KeyRelease 37 -> stop (<)

        --right arrow
        KeyPress 39 -> accelerateTo horizontalVelocity
        KeyRelease 39 -> stop (>)

        _ -> identity




accelerateTo velocity model =
  let tank1 = model.tank in
      { model | tank = {tank1 | groundAcceleration = velocity}}


--sets the deceleration of the tank once the left/right arrow is let go
stop velocity model =
  let tank1 = model.tank in
      { model | tank = {tank1 | groundAcceleration = 0}}

tick model =
    model
        --|> kill
        |> acceleration
        |> alienMovement
      --  |> bulletVelocity
      --  |> bulletMovement
        |> gravity
        |> motion
        |> floor
-- alienHitBox model =

gravity model =
  let alien = model.alien in
  {model | alien = {alien | vy = model.alien.vy - gravityAcceleration}}

motion model =
  let tank1 = model.tank in
    {model | tank = {tank1 | x = model.tank.x + model.tank.vx,
                            y = model.tank.y + model.tank.vy}
  }
acceleration model =
  let tank1 = model.tank in
    {model | tank = {tank1 | vx = model.tank.groundAcceleration}}

floor model =
    if model.tank.y < 0 then
      let tank1 = model.tank in
       {model | tank = {tank1 | y = 0, vy = 0}}
    else
       model


view model =
    toHtml(
      collage 1000 500 [(
        (moveY (model.tank.y - 200) (moveX (model.tank.x - 0) (toForm (image 100 100 "Tank.png"))))),
        (moveY -250 (filled (black ) (rect 1000 20))),
        (moveY (model.alien.y) (moveX (model.alien.x) (toForm (image 100 100 "Alien.png")))),
        (moveY (model.alien.y) (moveX (model.alien.x + 200) (toForm (image 100 100 "Alien.png")))),
        (moveY (model.alien.y) (moveX (model.alien.x - 200) (toForm (image 100 100 "Alien.png")))),
        (moveY (model.alien.y) (moveX (model.alien.x - 400) (toForm (image 100 100 "Alien.png")))),
        (moveY (model.alien.y) (moveX (model.alien.x + 400) (toForm (image 100 100 "Alien.png")))),
                (moveY (model.alien.y + 400) (moveX (model.alien.x + 100) (toForm (image 100 100 "Alien.png")))),
                (moveY (model.alien.y + 400) (moveX (model.alien.x - 100) (toForm (image 100 100 "Alien.png")))),
                (moveY (model.alien.y + 400) (moveX (model.alien.x - 300) (toForm (image 100 100 "Alien.png")))),
                (moveY (model.alien.y + 400) (moveX (model.alien.x + 300) (toForm (image 100 100 "Alien.png")))),
        (moveY ((model.tank.y - 155)) (moveX model.tank.x (filled (black ) (rect 10 7)))),
        -- alien is created and only moves opposite the tank for now
         (moveY 25 ( toForm (centered (fromString ("Welcome to Elm Invader!"))))),
         ( toForm (centered (fromString ("Press space to begin and left/right arrows to control tank and the up arrow to shoot"))))
        -- adds text instructions for now
        ]
        )
