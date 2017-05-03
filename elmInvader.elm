--Elm Invader Final Project Final Submission, Bridger Fisher & Ian Sime
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
import Array
import List exposing (..)

--initialize the model coordinates
initialModel = { x = 0,
                 y = 0,
                 vx = 0,
                 vy = 0,
               astronaut = initialAstronaut,
               alien = initialAlien,
               rocket = initialRocket,
               groundAcceleration = 0}

main = program
    { init = (initialModel, Cmd.none),
      view = view,
      subscriptions = subscriptions,
      update = updateWithCommand
    }
--States we can use
type State =
    NoAliensCaptured
    |OneAlienCaptured
    |TwoAlienCaptured
    |ThreeAlienCaptured
    |NoMoreLives
    |Win

--set up the subscriptions for the keypresses and time
subscriptions model =
    Sub.batch
    [ Keyboard.downs (KeyPress >> KeyMsg)
    , Keyboard.ups (KeyRelease >> KeyMsg),
    AnimationFrame.diffs Tick
    ]

--types
type Msg = KeyMsg Key | Tick Time
type Key = KeyRelease Int | KeyPress Int

updateWithCommand msg model =
  (update msg model, Cmd.none)

-- update the model after time and/or keypresses
update msg model =
    case msg of
      Tick time -> tick model
      KeyMsg k -> key k model

--initialize the type astronaut
type alias Astronaut = {
    x : Int,
    y : Int,
    vx : Int,
    vy : Int,
    groundAcceleration : Float,
    aliensCaptured : Int,
    lives : Int,
    missionComplete : Bool}

--initialize the astronaut
initialAstronaut = {
    x = 0,
    y = 0,
    vx = 0,
    vy = 0,
    groundAcceleration = 0.0,
    aliensCaptured = 0,
    lives = 2,
    missionComplete = False}

--sets up the jump of the astronaut
astronautJump model =
    if model.astronaut.vy == 0 then
      let astro = model.astronaut in
        { model | astronaut =
          {astro | vy = jumpVelocity}}

    else if model.vy == jumpVelocity then
      let astro = model.astronaut in
        { model | astronaut =
          {astro | y = model.astronaut.y + model.astronaut.vy}}

    else
        model

--sets up the motion of the astronaut
motion model =
  let astro = model.astronaut in
    { model | astronaut =
      {astro | x = model.astronaut.x + model.astronaut.vx,
               y = model.astronaut.y + model.astronaut.vy}}

--sets up acceleration of the astronaut
astronautAcceleration velocity model =
  let astro = model.astronaut in
      { model | astronaut = {astro | groundAcceleration = velocity}}

--sets the deceleration of the astronaut once the left/right arrow is let go
stop compare model =
      let astro = model.astronaut in
        {model | astronaut = {astro | groundAcceleration = 0}}

--sets up the traction of the astronaut on the moon's surface
traction model =
    if model.astronaut.y == 0 then
      let astro = model.astronaut in
        {model | astronaut = {astro | vx = model.astronaut.groundAcceleration}}
    else
      model

--sets up the effect of lower gravity
gravity model =
  let astro = model.astronaut in
    {model | astronaut = {astro | vy = model.astronaut.vy + gravityAcceleration}}

--initialize alien type
type alias Alien = {
    y : Int,
    x : Int,
    alive : Bool
}

--initialize alien
initialAlien = {
  y = 0,
  x = 0,
  alive = True}

--initialize rocket type
type alias Rocket = {
    x : Int,
    y : Int,
    vx : Int,
    vy : Int,
    groundAcceleration : Float,
    aliensCaptured : Int}

--initialize rocket
initialRocket = {
    x = 0,
    y = -300,
    vx = 0,
    vy = 0,
    groundAcceleration = 0.0,
    aliensCaptured = 0}

--set up variables
horizontalVelocity = 2
jumpVelocity = 2.5
gravityAcceleration = -0.028
groundAcceleration = 0

--key stroke updates
key msg model =
  model
    |> case msg of

       --up arrow
        KeyPress 38 -> astronautJump

        --left arrow
        KeyPress 37 -> astronautAcceleration -horizontalVelocity
        KeyRelease 37 -> stop (>)

        --right arrow
        KeyPress 39 -> astronautAcceleration horizontalVelocity
        KeyRelease 39 -> stop (<)

        _ -> identity
{--
checkAliensCaptured model =
    if (model.astronaut.aliensCaptured == 5) then
      {model | state = AllAliensCaptured}
    else
      model
--}

--update on ticks
tick model =
    model
        |> traction
        |> gravity
        |> motion
        |> floor
        |> leftSide
        |> rightSide
        --|> alienGroundCollision
        |> platform1
        |> platform2
        |> platform3
        |> platform4
        |> platform5
        |> platform6
        |> portalIn
        |> alienShip1
        |> alienShip2
        |> captureAlien1
        |> captureAlien2
        |> captureAlien3
      --  |> captureAlien4
      --  |> captureAlien5
        |> missionSuccessful

--set up the floor boundary
floor model =
    if model.astronaut.y < 0 then
      let astro = model.astronaut in
        { model | astronaut =
          {astro | y = 0, vy = 0}}
    else
       model

--set up the left boundary
leftSide model =
    if model.astronaut.x < -500 then
      let astro = model.astronaut in
        { model | astronaut =
          {astro | x = -500, vx = 0}}

    else
       model

--set up the right boundary
rightSide model =
    if model.astronaut.x > 500 then
      let astro = model.astronaut in
        { model | astronaut =
          {astro | x = 500, vx = 0}}

    else
       model
{--
alienGroundCollision model =
  if (model.astronaut.x) < -300 && (model.astronaut.y) == 0 then
    let astro = model.astronaut in
      { model | astronaut =
        {astro | missionComplete =  True}}
  else
    model
--}

--set up the in portal
portalIn model =
  if (model.astronaut.y) == 0 && (model.astronaut.x) > 475 then
    let astro = model.astronaut in
      { model | astronaut =
        {astro | x = 450, y = 425, vx = 0, vy = 0}}
  else
    model

--set up the platform1
platform1 model =
  if (model.astronaut.y) > 90 && (model.astronaut.y) < 100 && (model.astronaut.x) < -170 && (model.astronaut.x) > -270 then
    let astro = model.astronaut in
      { model | astronaut =
        {astro | x = model.astronaut.x, y = model.astronaut.y, vx = model.astronaut.groundAcceleration , vy = 0}}

  else
    model

--set up the platform2
platform2 model =
  if (model.astronaut.y) > 90 && (model.astronaut.y) < 100 && (model.astronaut.x) < 260 && (model.astronaut.x) > 160 then
      let astro = model.astronaut in
        { model | astronaut =
          {astro | x = model.astronaut.x, y = model.astronaut.y, vx = model.astronaut.groundAcceleration , vy = 0}}

  else
    model

--set up the platform3
platform3 model =
    if (model.astronaut.y) > 190 && (model.astronaut.y) < 200 && (model.astronaut.x) < 500 && (model.astronaut.x) > 400 then
      let astro = model.astronaut in
        { model | astronaut =
          {astro | x = model.astronaut.x, y = model.astronaut.y, vx = model.astronaut.groundAcceleration , vy = 0}}

    else
      model

--set up the platform4
platform4 model =
    if (model.astronaut.y) > 405 && (model.astronaut.y) < 415 && (model.astronaut.x) < 500 && (model.astronaut.x) > 400 then
      let astro = model.astronaut in
        { model | astronaut =
          {astro | x = model.astronaut.x, y = model.astronaut.y, vx = model.astronaut.groundAcceleration , vy = 0}}

    else
      model

--set up the platform5
platform5 model =
  if (model.astronaut.y) > 290 && (model.astronaut.y) < 300 && (model.astronaut.x) < 240 && (model.astronaut.x) > 190 then
      let astro = model.astronaut in
        { model | astronaut =
          {astro | x = model.astronaut.x, y = model.astronaut.y, vx = model.astronaut.groundAcceleration , vy = 0}}

  else
    model

--set up the platform6
platform6 model =
  if (model.astronaut.y) > 290 && (model.astronaut.y) < 300 && (model.astronaut.x) < -100 && (model.astronaut.x) > -260 then
    let astro = model.astronaut in
      { model | astronaut =
        {astro | x = model.astronaut.x, y = model.astronaut.y, vx = model.astronaut.groundAcceleration , vy = 0}}

  else
    model

--set up the alienship1
alienShip1 model =
  if (model.astronaut.y) > 175 && (model.astronaut.y) < 300 && (model.astronaut.x) < 10 && (model.astronaut.x) > -100 then
    let astro = model.astronaut in
      { model | astronaut =
        {astro | x = 0,
                 y = 0,
                lives = (model.astronaut.lives - 1)}}

  else
    model

--set up the alienship2
alienShip2 model =
  if (model.astronaut.y) > 250 && (model.astronaut.y) < 350 && (model.astronaut.x) < 600 && (model.astronaut.x) > 400 then
    let astro = model.astronaut in
      { model | astronaut =
        {astro | x = 0,
                 y = 0,
                lives = (model.astronaut.lives - 1)}}

  else
    model

--set up the capture alien1
captureAlien1 model =
  if (model.astronaut.y) == 0 && (model.astronaut.x) < 203 && (model.astronaut.x) > 200 && (model.astronaut.aliensCaptured) == 0 then
  --  {model | state = OneAlienCaptured}
    let astro = model.astronaut in
      { model | astronaut =
        {astro | aliensCaptured = (model.astronaut.aliensCaptured + 1)}}

  else
    model

--set up the capture alien3
captureAlien3 model =
  if (model.astronaut.y) > 290 && (model.astronaut.y) < 300 && (model.astronaut.x) < -157 && (model.astronaut.x) > -162 && (model.astronaut.aliensCaptured) == 2 then
    --{model | state = TwoAlienCaptured}
    let astro = model.astronaut in
      { model | astronaut =
        {astro | aliensCaptured = (model.astronaut.aliensCaptured + 1)}}

  else
    model

--set up the capture alien2
captureAlien2 model =
  if (model.astronaut.y) > 90 && (model.astronaut.y) < 100 && (model.astronaut.x) < -212 && (model.astronaut.x) > -215 && (model.astronaut.aliensCaptured) == 1 then
    --{model | state = ThrAlienCaptured}
    let astro = model.astronaut in
      { model | astronaut =
        {astro | aliensCaptured = (model.astronaut.aliensCaptured + 1)}}

  else
    model

--check if the mission is a success
missionSuccessful model =
  if (model.astronaut.x) < -300 && (model.astronaut.aliensCaptured) == 3 then
          let astro = model.astronaut in
            { model | astronaut =
              {astro | missionComplete = True}}
  else
    model

asText value = (show value)

view model =
  if model.astronaut.lives == 0 then
    toHtml(
      collage 1000 500 [
        (toForm (image 1000 500 "spaceBackground2.jpg")),
        (moveY (0) (moveX (0) (toForm (leftAligned (Text.color (red )(fromString ("You Died! Mission Failure!")))))))

        ])

  else if model.astronaut.missionComplete == True then
    toHtml(
      collage 1000 500 [
        (toForm (image 1000 500 "spaceBackground2.jpg")),
        (moveY (0) (moveX (0) (toForm (leftAligned (Text.color (red )(fromString ("Mission Successful!  Good hunting!"))))))),
        (moveY (0) (moveX (-400) (toForm (image 200 500 "rocketLaunch.gif"))))
        ])

  else if model.astronaut.aliensCaptured == 0 then
    toHtml(
      collage 1000 500 [
        (toForm (image 1000 500 "spaceBackground2.jpg")),
        --(moveY (model.astronaut.y - 200) (moveX (model.astronaut.x - 0) (toForm (image 100 100 "Tank.png")))),
        (moveY -150 (moveX (-215) (filled (grey ) (rect 100 10)))),   --platform1
        (moveY -150 (moveX (215) (filled (grey ) (rect 100 10)))),    --platform2
        (moveY -50 (moveX (450) (filled (grey ) (rect 100 10)))),    --platform3
        (moveY 170 (moveX (450) (filled (grey ) (rect 100 10)))),    --platform4
        (moveY 50 (moveX (225) (filled (grey ) (rect 100 10)))),    --platform5
        (moveY 50 (moveX (-165) (filled (grey ) (rect 100 10)))),   --platform6
        (moveY (-210) (moveX (475) (toForm (image 75 75 "inPortal.png")))),  --inPortal
        (moveY (210) (moveX (475) (toForm (image 75 75 "outPortal.png")))),  --outPortal
        (moveY (model.astronaut.y - 210) (moveX (model.astronaut.x - 0) (toForm (image 75 75 "astronaut2.png")))),
        (moveY (0) (moveX (-400) (toForm (image 200 500 "rocketLanding.gif")))),
        (moveY (50) (moveX (15) (toForm (image 100 100 "alienRay.png")))), --alienRay1
        (moveY (100) (moveX (350) (toForm (image 100 100 "alienRay.png")))), --alienRay2
        (moveY (-210) (moveX (215) (toForm (image 100 100 "alienCapture.png")))), --alienCapture1
        --(moveY (-105) (moveX (-215) (toForm (image 100 100 "alienCapture.png")))), --alienCapture2
        --(moveY (95) (moveX (-160) (toForm (image 100 100 "alienCapture.png")))), --alienCapture3
        (moveY (240) (moveX (-400) (toForm (leftAligned (fromString ("Aliens Captured: ")))))),
        (moveY (240) (moveX (-335) ( toForm ((asText model.astronaut.aliensCaptured))))),
        (moveY (215) (moveX (-400) (toForm (leftAligned (fromString ("Lives: ")))))),
        (moveY (215) (moveX (-375) ( toForm ((asText model.astronaut.lives))))),
        (moveY (230) (moveX (100) (toForm (leftAligned (Text.color (red )(fromString ("Welcome to Elm Invader! Use the arrow keys to control the astronaut and catch all the aliens.")))))))
        ])
        -- (moveY 115 ( toForm (centered (fromString ("Welcome to Elm Invader!"))))),
      --   (moveY 95 ( toForm (centered (fromString ("Player 1 press left/right arrows to control tank and the up arrow to shoot")))))
        -- adds text instructions for now

    else if model.astronaut.aliensCaptured == 1 then
      toHtml(
        collage 1000 500 [
          (toForm (image 1000 500 "spaceBackground2.jpg")),
          --(moveY (model.astronaut.y - 200) (moveX (model.astronaut.x - 0) (toForm (image 100 100 "Tank.png")))),
          (moveY -150 (moveX (-215) (filled (grey ) (rect 100 10)))),   --platform1
          (moveY -150 (moveX (215) (filled (grey ) (rect 100 10)))),    --platform2
          (moveY -50 (moveX (450) (filled (grey ) (rect 100 10)))),    --platform3
          (moveY 170 (moveX (450) (filled (grey ) (rect 100 10)))),    --platform4
          (moveY 50 (moveX (225) (filled (grey ) (rect 100 10)))),    --platform5
          (moveY 50 (moveX (-165) (filled (grey ) (rect 100 10)))),   --platform6
          (moveY (-210) (moveX (475) (toForm (image 75 75 "inPortal.png")))),  --inPortal
          (moveY (210) (moveX (475) (toForm (image 75 75 "outPortal.png")))),  --outPortal
          (moveY (model.astronaut.y - 210) (moveX (model.astronaut.x - 0) (toForm (image 75 75 "astronaut2.png")))),
          (moveY (0) (moveX (-400) (toForm (image 200 500 "rocketLanding.gif")))),
          (moveY (50) (moveX (15) (toForm (image 100 100 "alienRay.png")))), --alienRay1
          (moveY (100) (moveX (350) (toForm (image 100 100 "alienRay.png")))), --alienRay2
          --(moveY (-210) (moveX (215) (toForm (image 100 100 "alienCapture.png")))), --alienCapture1
          (moveY (-105) (moveX (-215) (toForm (image 100 100 "alienCapture.png")))), --alienCapture2
          --(moveY (95) (moveX (-160) (toForm (image 100 100 "alienCapture.png")))), --alienCapture3
          (moveY (240) (moveX (-400) (toForm (leftAligned (fromString ("Aliens Captured: ")))))),
          (moveY (240) (moveX (-335) ( toForm ((asText model.astronaut.aliensCaptured))))),
          (moveY (215) (moveX (-400) (toForm (leftAligned (fromString ("Lives: ")))))),
          (moveY (215) (moveX (-375) ( toForm ((asText model.astronaut.lives))))),
          (moveY (230) (moveX (100) (toForm (leftAligned (Text.color (red )(fromString ("Welcome to Elm Invader! Use the arrow keys to control the astronaut and catch all the aliens.")))))))
          ])

      else if model.astronaut.aliensCaptured == 2 then
        toHtml(
          collage 1000 500 [
            (toForm (image 1000 500 "spaceBackground2.jpg")),
            --(moveY (model.astronaut.y - 200) (moveX (model.astronaut.x - 0) (toForm (image 100 100 "Tank.png")))),
            (moveY -150 (moveX (-215) (filled (grey ) (rect 100 10)))),   --platform1
            (moveY -150 (moveX (215) (filled (grey ) (rect 100 10)))),    --platform2
            (moveY -50 (moveX (450) (filled (grey ) (rect 100 10)))),    --platform3
            (moveY 170 (moveX (450) (filled (grey ) (rect 100 10)))),    --platform4
            (moveY 50 (moveX (225) (filled (grey ) (rect 100 10)))),    --platform5
            (moveY 50 (moveX (-165) (filled (grey ) (rect 100 10)))),   --platform6
            (moveY (-210) (moveX (475) (toForm (image 75 75 "inPortal.png")))),  --inPortal
            (moveY (210) (moveX (475) (toForm (image 75 75 "outPortal.png")))),  --outPortal
            (moveY (model.astronaut.y - 210) (moveX (model.astronaut.x - 0) (toForm (image 75 75 "astronaut2.png")))),
            (moveY (0) (moveX (-400) (toForm (image 200 500 "rocketLanding.gif")))),
            (moveY (50) (moveX (15) (toForm (image 100 100 "alienRay.png")))), --alienRay1
            (moveY (100) (moveX (350) (toForm (image 100 100 "alienRay.png")))), --alienRay2
            --(moveY (-210) (moveX (215) (toForm (image 100 100 "alienCapture.png")))), --alienCapture1
            --(moveY (-210) (moveX (215) (toForm (image 100 100 "alienCapture.png")))), --alienCapture2
            (moveY (95) (moveX (-160) (toForm (image 100 100 "alienCapture.png")))), --alienCapture3
            (moveY (240) (moveX (-400) (toForm (leftAligned (fromString ("Aliens Captured: ")))))),
            (moveY (240) (moveX (-335) ( toForm ((asText model.astronaut.aliensCaptured))))),
            (moveY (215) (moveX (-400) (toForm (leftAligned (fromString ("Lives: ")))))),
            (moveY (215) (moveX (-375) ( toForm ((asText model.astronaut.lives))))),
            (moveY (230) (moveX (100) (toForm (leftAligned (Text.color (red )(fromString ("Welcome to Elm Invader! Use the arrow keys to control the astronaut and catch all the aliens.")))))))
            ])

      else if model.astronaut.aliensCaptured == 3 then
        toHtml(
          collage 1000 500 [
            (toForm (image 1000 500 "spaceBackground2.jpg")),
            --(moveY (model.astronaut.y - 200) (moveX (model.astronaut.x - 0) (toForm (image 100 100 "Tank.png")))),
            (moveY -150 (moveX (-215) (filled (grey ) (rect 100 10)))),   --platform1
            (moveY -150 (moveX (215) (filled (grey ) (rect 100 10)))),    --platform2
            (moveY -50 (moveX (450) (filled (grey ) (rect 100 10)))),    --platform3
            (moveY 170 (moveX (450) (filled (grey ) (rect 100 10)))),    --platform4
            (moveY 50 (moveX (225) (filled (grey ) (rect 100 10)))),    --platform5
            (moveY 50 (moveX (-165) (filled (grey ) (rect 100 10)))),   --platform6
            (moveY (-210) (moveX (475) (toForm (image 75 75 "inPortal.png")))),  --inPortal
            (moveY (210) (moveX (475) (toForm (image 75 75 "outPortal.png")))),  --outPortal
            (moveY (model.astronaut.y - 210) (moveX (model.astronaut.x - 0) (toForm (image 75 75 "astronaut2.png")))),
            (moveY (0) (moveX (-400) (toForm (image 200 500 "rocketLanding.gif")))),
            (moveY (50) (moveX (15) (toForm (image 100 100 "alienRay.png")))), --alienRay1
            (moveY (100) (moveX (350) (toForm (image 100 100 "alienRay.png")))), --alienRay2
            --(moveY (-210) (moveX (215) (toForm (image 100 100 "alienCapture.png")))), --alienCapture1
            --(moveY (-210) (moveX (215) (toForm (image 100 100 "alienCapture.png")))), --alienCapture2
            --(moveY (95) (moveX (-160) (toForm (image 100 100 "alienCapture.png")))), --alienCapture3
            (moveY (240) (moveX (-400) (toForm (leftAligned (fromString ("Aliens Captured: ")))))),
            (moveY (240) (moveX (-335) ( toForm ((asText model.astronaut.aliensCaptured))))),
            (moveY (215) (moveX (-400) (toForm (leftAligned (fromString ("Lives: ")))))),
            (moveY (215) (moveX (-375) ( toForm ((asText model.astronaut.lives))))),
            (moveY (190) (moveX (-375) (toForm (leftAligned (Text.color (red )(fromString ("Aliens Captured! Get to the ship!")))))))
            ])

    else
      toHtml(
        collage 1000 500 [
          (toForm (image 1000 500 "spaceBackground2.jpg")),
          (moveY (0) (moveX (0) (toForm (leftAligned (Text.color (red )(fromString ("Mission Failure!")))))))])
