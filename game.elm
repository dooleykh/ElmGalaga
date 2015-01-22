import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Signal (..)
import Window
import List
import Debug
import Keyboard
import Transform2D
import Time (..)
import Random
{-

For your final Elm project, I'm going to ask you to build a video game
in elm that's a little more sophisticated than Spacewar.

You have 2 options, one of which is pretty close ended and one of
which is open ended:

OPTION 1:  Make a classic-style scrolling shooter game

My personal nostalgic example of this genre is Raptor: Call of the
Shadows.  You can see it here:

https://www.youtube.com/watch?v=ZUfeVcIg3Mg

Here are the requirements:

1.  Graphics are not important, except insofar as they communicate
gameplay elements.  If you want this game to be about heroic green
triangles and their attempt to defend their homeland from the nefarious
red,yellow,and blue triangles I'm fine with that.

But if I can't tell what's a power-up, or all the weapons seem the
same because I can't see how they act differently - that's bad.

2. [33 points] Your game should have the distinctive elements of the
scrolling shooter genre:

   a.  A background that automatically slowly scrolls (could just be
       stars or clouds or something)

   b.  A flying ship (that's you) that shoots

   c.  Tons of enemies that fly around (usually in prearranged patterns,
       coming and going at specific times)

   d.  You can blow up the enemies, your enemies can blow you up.
       Crashing into enemies blows them up and at least damages you.

3. [33 points] At least 5 qualitatively different weapons.  These
should not just be different color weapons that do different amounts
of damage.  At the very least, they should fire in highly different
patterns, spawn additional sub-bullets etc.  At best, they should act
differently (area of effect, damage over time, homing, etc.).  The
weapon effects should be clear visibly (obvious for homing, but damage
over time might need a halo effect or something).

Make a cheat or something so I can try all the weapons without getting
tons of powerups.

4. [33 points] At least 5 qualitatively different enemies.  Again,
more different than just different amounts of life.  At the very
least, different patterns of movement and different numbers of bullet
sources/times for shooting.  At best, some simplistic AI or different
attack effects.

The enemies should be visibly distinct.

OPTION 2: Make a copy of an existing game, or in a genre of your choice

The key requirement here is that things be at least as difficult as
OPTION 1.  You'll also need to have something generic (like weapons or
enemies above) that you have to have different kinds of - because that
will force you do some functional-style abstraction.

You have to get approval from me to do this.  If you want approval,
make up a point breakdown for your game and send it to me.

-}

-- Type Definitions
type EnemyType = Fighter | Diver | Drifter | Hunter | Spawner | Spinner
type BulletType = StandardBullet | Bomb | SpawnerBullet
type GunType = StandardGun | BombLauncher | WShot | SpawnerLauncher | Beam
type PowerupType = GunStandard | GunBomb | GunW | GunSpawner | GunBeam | Health
type alias Hero = {x:Float, y:Float, fireDelay:Int, gunType:GunType, health: Int}
type alias Enemy = {x:Float, y:Float, enemyType:EnemyType, fireDelay:Int, health: Int}
type alias Bullet = {x:Float, y:Float, angle:Float, bulletType:BulletType, tick:Int}
type alias Powerup = {x:Float, y:Float, powerupType:PowerupType}

type alias GameState = {hero:Hero, enemies: List Enemy, heroBullets: List Bullet, enemyBullets: List Bullet, powerups: List Powerup}

type alias Input = {arrows:Arrows, delta:Time, fire:Bool, power:Int}
type alias Arrows = {x:Float, y:Float}

-- 'Constructors'
hero: Float -> Float -> Hero
hero xStart yStart =
  {x=xStart, y=yStart, fireDelay=0, gunType=StandardGun, health=10}

enemy: Float -> Float -> EnemyType -> Enemy
enemy xStart yStart enemyType =
  case enemyType of
    Fighter -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=5, health=1}
    Diver -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=Random.maxInt, health=5}
    Drifter -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=3, health=2}
    Hunter -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=10, health=3}
    Spawner -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=30, health=10}
    Spinner -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=40, health=5}

bullet: Float -> Float -> Float -> BulletType -> Bullet
bullet xStart yStart angle bType =
  let record = {x=xStart, y=yStart, angle=angle, bulletType=bType, tick=0}
  in
    case bType of
      StandardBullet -> record
      Bomb -> {record | tick <- 50}
      SpawnerBullet -> {record | tick <- 30}

powerup : Float -> Float -> PowerupType -> Powerup
powerup xStart yStart pType =
  {x=xStart, y=yStart, powerupType=pType}

gameState : GameState
gameState =
  let h = hero 0.0 0.0
      e = [enemy 50.0 50.0 Hunter]
  in
    {hero = h, enemies = e, heroBullets = [], enemyBullets = [], powerups = []}

--Update Functions
updateGameState = foldp updateGame gameState input

input : Signal Input
input =
  let floatify {x,y} = { x = toFloat x, y = toFloat y }
      --TODO Specific key click for powerup
      pwerup keycodes = if (List.any (\key -> key == 13) keycodes) then 1 else 0
  in
      sampleOn delta <| Input <~ (map floatify Keyboard.arrows)
                              ~ delta
                              ~ Keyboard.space
                              ~ (map pwerup Keyboard.keysDown)
delta =
  map inSeconds (fps 30)

updateGame : Input -> GameState -> GameState
updateGame input state =
  {state |
    hero <- updateHero input state,
    enemies <- updateEnemies state,
    heroBullets <- updateHeroBullets input state,
    enemyBullets <- updateEnemyBullets state,
    powerups <- updatePowerups state
  }

updateHero : Input -> GameState -> Hero
updateHero inputs state =
  state.hero

updateEnemies : GameState -> List Enemy
updateEnemies state=
  state.enemies

updateHeroBullets : Input -> GameState -> List Bullet
updateHeroBullets inputs state =
  state.heroBullets

updateEnemyBullets : GameState -> List Bullet
updateEnemyBullets state =
  state.enemyBullets

updatePowerups : GameState -> List Powerup
updatePowerups state =
  state.powerups

--View functions

isoceles : Float -> Float -> Shape
isoceles w h =
  let w2 = w/2.0
      h2 = h/2.0
  in polygon [(h2,0.0),(-h2,w2),(-h2,-w2)]

drawFighter : Form
drawFighter =
  let triangle = filled red (ngon 3 10)
  in group [moveX -5 triangle,moveX 5 triangle]

drawDiver : Form
drawDiver =
  filled red (isoceles 15 25)

drawDrifter : Form
drawDrifter =
  let triangle = filled red (ngon 3 10)
  in group [moveY -5 triangle,moveY 5 triangle]

drawHunter : Form
drawHunter =
  let triangle = filled red (ngon 3 10)
      sideTriangle = filled red (isoceles 10 20)
  in group [moveX -5 triangle,moveX 5 triangle, moveY -10 sideTriangle, moveY 10 sideTriangle]

drawSpawner : Form
drawSpawner =
  let triangle = filled red (ngon 3 10)
      shiftY = 5*sqrt(3)
      shiftX = 5
  in group [move (shiftX,shiftY) triangle, move (shiftX,-shiftY) triangle, move (20,0) triangle]

drawSpinner : Form
drawSpinner =
  let triangle = filled red (ngon 3 7)
      shiftValue = 7/2*(1+sqrt(3))
  in group [moveX shiftValue triangle,
            (rotate 3.14 (moveX -shiftValue triangle)),
            (rotate 1.57 (moveY shiftValue triangle)),
            (rotate -1.57 (moveY -shiftValue triangle))]

drawHero : Form
drawHero =
  let triangle = filled lightBlue (ngon 3 10)
      mainBody = filled lightBlue (isoceles 20 30)
  in group [move (-15,10) triangle,
            move (-15,-10) triangle,
            mainBody]

-- Code for Test Drawing a single ship onto the board at x,y coords
viewGameState : (Int, Int) -> GameState -> Element
viewGameState (w, h) state =
  let heroForm = viewHero state.hero
      enemyForms = List.map viewEnemy state.enemies
      merged = collage w h ([heroForm] ++ enemyForms)
  in
    layers [fittedImage w h "/starfield.gif", merged]



viewHero : Hero -> Form
viewHero hero =
  drawHero |> move (hero.x, hero.y)

viewEnemy : Enemy -> Form
viewEnemy enemy =
  let form = case enemy.enemyType of
                Fighter -> drawFighter
                Diver -> drawDiver
                Drifter -> drawDrifter
                Hunter -> drawHunter
                Spawner -> drawSpawner
                Spinner -> drawSpinner
  in form |> rotate 3.14 |> move (enemy.x, enemy.y)

viewShip : (Float,Float) -> (Int, Int) -> Form -> Element
viewShip (x,y) (w,h) ship =
  let shipImage =
    ship
    |> rotate 3.14
    |> move (x,y)
  in
    collage w h [shipImage]

-- Starfield gif can be obtained at http://30000fps.com/post/93334443098
view : (Int, Int) -> Element
view (w, h) =
  layers [fittedImage w h "/starfield.gif", viewShip (((toFloat w)/2.0-40),0) (w,h) drawHunter, viewShip (((toFloat -w)/2.0+40),0) (w,h) drawHero]

main =
  map2 viewGameState Window.dimensions updateGameState
