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
type alias Hero = {x:Float, y:Float, fireDelay:Int, gunType:GunType, health: Int, speed: Float}
type alias Enemy = {x:Float, y:Float, enemyType:EnemyType, fireDelay:Int, health: Int, xStart: Float, yStart: Float, angle: Float}
type alias Bullet = {x:Float, y:Float, angle:Float, bulletType:BulletType, tick:Int}
type alias Powerup = {x:Float, y:Float, powerupType:PowerupType}

type alias GameState = {hero:Hero, enemies: List Enemy, heroBullets: List Bullet, enemyBullets: List Bullet, powerups: List Powerup, dimensions: (Int, Int), seed: Random.Seed, gameOver: Bool}

type alias Input = {arrows:Arrows, delta:Time, fire:Bool, keysDown: List Int, dimensions:(Int, Int)}
type alias Arrows = {x:Float, y:Float}

-- 'Constructors'
hero: Float -> Float -> Hero
hero xStart yStart =
  {x=xStart, y=yStart, fireDelay=0, gunType=StandardGun, health=10, speed=5.0}

enemy: Float -> Float -> EnemyType -> Enemy
enemy xStart yStart enemyType =
  case enemyType of
    Fighter -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=5, health=1, xStart=xStart, yStart=yStart, angle=0}
    Diver -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=Random.maxInt, health=2, xStart=xStart, yStart=yStart, angle=0}
    Drifter -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=3, health=2, xStart=xStart, yStart=yStart, angle=0}
    Hunter -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=10, health=3, xStart=xStart, yStart=yStart, angle=0}
    Spawner -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=20, health=10, xStart=xStart, yStart=yStart, angle=0}
    Spinner -> {x=xStart, y=yStart, enemyType=enemyType, fireDelay=15, health=3, xStart=xStart, yStart=yStart, angle=0}

bullet: Float -> Float -> Float -> BulletType -> Bullet
bullet xStart yStart angle bType =
  let record = {x=xStart, y=yStart, angle=angle, bulletType=bType, tick=0}
  in
    case bType of
      StandardBullet -> record
      Bomb -> {record | tick <- 30}
      SpawnerBullet -> {record | tick <- 20}

powerup : Float -> Float -> PowerupType -> Powerup
powerup xStart yStart pType =
  {x=xStart, y=yStart, powerupType=pType}

gameState : GameState
gameState =
  let h = hero 0.0 0.0
      e = []
  in
    {hero = h, enemies = e, heroBullets = [], enemyBullets = [], powerups = [],dimensions=(0,0),seed=Random.initialSeed 165411,gameOver=False}

--Update Functions
updateGameState = foldp updateGame gameState input

input : Signal Input
input =
  let floatify {x,y} = { x = toFloat x, y = toFloat y }
  in
      sampleOn delta <| Input <~ (map floatify Keyboard.arrows)
                              ~ delta
                              ~ Keyboard.space
                              ~ Keyboard.keysDown
                              ~ Window.dimensions
delta =
  map inSeconds (fps 30)

updateGame : Input -> GameState -> GameState
updateGame input state =
  if (state.gameOver) then state
  else
  let movedState = {state |
        hero <- updateHero input state,
        enemies <- updateEnemies input state,
        heroBullets <- updateHeroBullets input state,
        enemyBullets <- updateEnemyBullets state,
        powerups <- updatePowerups state,
        dimensions <- input.dimensions,
        seed <- nextSeed state.seed
      }
      collidedState = {movedState |
        hero <- (collideHeroWithEnemies movedState.enemies movedState.hero) |> collideHeroWithBullets movedState.enemyBullets,
        enemies <- (collideEnemiesWithHero movedState.hero movedState.enemies) |> collideEnemiesWithBullets movedState.heroBullets,
        heroBullets <- heroRemainingBullets movedState.enemies movedState.heroBullets,
        enemyBullets <- enemyRemaingBullets movedState.hero movedState.enemyBullets
      }
      removedState = if (collidedState.hero.health <= 0) then {collidedState | gameOver <- True} else collidedState
  in
    removedState

manhattan : (Float, Float) -> (Float, Float) -> Float
manhattan (x, y) (x2, y2) =
  let xDist = if (x > x2) then x-x2 else x2-x
      yDist = if (y > y2) then y-y2 else y2-y
  in xDist+yDist

colliding : (Float,Float) -> (Float,Float) -> Bool
colliding (x1,y1) (x2,y2) =
  (manhattan (x1,y1) (x2,y2)) < 10 --TODO parameter

collideHeroWithEnemies : List Enemy -> Hero -> Hero
collideHeroWithEnemies enemies hero =
  let collidingEnemies = List.filter (\e -> colliding (e.x, e.y) (hero.x, hero.y)) enemies
      sumEnemyHealth = List.foldr (+) 0 (List.map (\e -> e.health) collidingEnemies)
  in
    {hero | health <- hero.health - sumEnemyHealth}

collideHeroWithBullets : List Bullet -> Hero -> Hero
collideHeroWithBullets bullets hero =
  let collidingBullets = List.filter (\b -> colliding (b.x, b.y) (hero.x, hero.y)) bullets
      sumBullets = List.length collidingBullets
  in
    {hero | health <- hero.health - sumBullets}

collideEnemiesWithHero : Hero -> List Enemy -> List Enemy
collideEnemiesWithHero hero enemies =
  List.filter (\e -> not (colliding (e.x, e.y) (hero.x, hero.y))) enemies

collideEnemiesWithBullets : List Bullet -> List Enemy -> List Enemy
collideEnemiesWithBullets bullets enemies =
  List.filter (\e -> e.health > 0) (List.map (collideEnemyWithBullets bullets) enemies)

collideEnemyWithBullets : List Bullet -> Enemy -> Enemy
collideEnemyWithBullets bullets enemy =
  let collidingBullets = List.filter (\b -> colliding (b.x, b.y) (enemy.x, enemy.y)) bullets
      sumBullets = List.length collidingBullets
  in
    {enemy | health <- enemy.health - sumBullets}

heroRemainingBullets : List Enemy -> List Bullet -> List Bullet
heroRemainingBullets enemies bullets =
  -- List.filter (\b -> (List.any (\e -> colliding (b.x, b.y) (e.x, e.y)) enemies)) bullets
  List.filter (\b -> not (bulletCollide b enemies)) bullets

bulletCollide : Bullet -> List Enemy -> Bool
bulletCollide bullet enemies =
  List.any (\e -> colliding (e.x, e.y) (bullet.x, bullet.y)) enemies

enemyRemaingBullets : Hero -> List Bullet -> List Bullet
enemyRemaingBullets hero enemyBullets =
  List.filter (\b -> not (colliding (hero.x, hero.y) (b.x, b.y))) enemyBullets

nextSeed : Random.Seed -> Random.Seed
nextSeed seed =
  let (unused, seed') = Random.generate (Random.float 0 1) seed
  in seed'

delayed : Int -> Bool
delayed count =
  count > 0

newDelay : Int-> Int -> Bool -> Int
newDelay newDelayValue currentCount triedToFire =
  if | not triedToFire -> (currentCount - 1)
     | not (delayed currentCount) -> newDelayValue
     | otherwise -> (currentCount - 1)

newBulletDelay : GunType -> Int
newBulletDelay gun =
  case gun of
    StandardGun -> 10
    BombLauncher -> 20
    WShot -> 10
    Beam -> 1
    SpawnerLauncher -> 20

updateHero : Input -> GameState -> Hero
updateHero inputs state =
  let localHero = state.hero
      newWeapon = determineNewGun localHero.gunType inputs.keysDown
      newDelayCount = newDelay (newBulletDelay localHero.gunType) localHero.fireDelay inputs.fire
      newX = localHero.x + inputs.arrows.x*localHero.speed
      newY = localHero.y + inputs.arrows.y*localHero.speed
  in
  if (heroInDemensions (newX, newY) state.dimensions) then
    {localHero | x <- newX, y <- newY, fireDelay <- newDelayCount, gunType <- newWeapon}
  else
    {localHero | fireDelay <- newDelayCount, gunType <- newWeapon}

determineNewGun : GunType -> List Int -> GunType
determineNewGun currentGun keysDown =
  let
      maxNum = List.foldr (max) 0
                               (List.filter (\code -> code >= 49 && code <= 53) keysDown)
  in
    case maxNum of
      0  -> currentGun
      49 -> StandardGun
      50 -> BombLauncher
      51 -> WShot
      52 -> Beam
      53 -> SpawnerLauncher

heroInDemensions : (Float, Float) -> (Int, Int) -> Bool
heroInDemensions (x, y) (windowX, windowY) =
  let (floatX, floatY) = (toFloat windowX, toFloat windowY)
  in
    (x >= (-floatX / 2) + 17 && x <= (floatX / 2) - 17) && (y >= (-floatY / 2) + 17 && y <= (floatY / 2) - 17)

updateEnemies : Input -> GameState -> List Enemy
updateEnemies input state =
  let x = toFloat (fst input.dimensions)
      y = toFloat (snd input.dimensions)
      spawned = spawnEnemies state.seed state.dimensions
      newEnemies = state.enemies ++ spawned
  in
    List.filter (enemyInDimensions (x, y))
                <| List.map (updateEnemy state) newEnemies

spawnEnemies : Random.Seed -> (Int, Int) -> List Enemy
spawnEnemies seed (windowX, windowY) =
  let maxX = (toFloat windowX)/2.0
      minX = maxX - 20.0
      maxY = (toFloat windowY)/2.0 - 17.0
      minY = -maxY
      (addValue, seed') = Random.generate (Random.float 0 1) seed
      (xStart, seed'') = Random.generate (Random.float minX maxX) seed'
      (yStart, seed_ignored) = Random.generate (Random.float minY maxY) seed''
  in
    if | addValue < 0.99 -> []
       | addValue < 0.992 -> [(enemy xStart yStart Fighter)]
       | addValue < 0.995 -> [(enemy xStart yStart Diver)]
       | addValue < 0.997 -> [(enemy xStart yStart Drifter)]
       | addValue < 0.998 -> [(enemy xStart yStart Hunter)]
       | addValue < 0.999 -> [(enemy xStart (yStart/2.0) Spinner)]
       | otherwise -> [(enemy xStart yStart Spawner)]

newEnemyFireDelay : EnemyType -> Int
newEnemyFireDelay eType =
  case eType of
    Fighter -> 15
    Diver -> Random.maxInt
    Drifter -> 13
    Hunter -> 20
    Spawner -> 90
    Spinner -> 50

updateEnemyFireDelay : Enemy -> Enemy
updateEnemyFireDelay enemy =
  if (not (delayed enemy.fireDelay)) then {enemy | fireDelay <- newEnemyFireDelay enemy.enemyType}
    else {enemy | fireDelay <- enemy.fireDelay - 1}

updateEnemy : GameState -> Enemy -> Enemy
updateEnemy state enemy =
  let newEnemy = updateEnemyFireDelay enemy
  in
    case enemy.enemyType of
      Fighter -> {newEnemy | x <- enemy.x - 3}
      Diver -> if | enemy.x < 0 -> (if | enemy.y > state.hero.y -> {newEnemy | y <- enemy.y - 5, x <- enemy.x - 7}
                                       | enemy.y < state.hero.y -> {newEnemy | y <- enemy.y + 5, x <- enemy.x - 7}
                                       | otherwise -> {newEnemy | x <- enemy.x - 7})
                  | otherwise -> {newEnemy | x <- enemy.x - 3}
      Drifter -> {newEnemy | x <- enemy.x - 3, y <- enemy.y + 12 * sin(enemy.x * 1/30)}
      Hunter -> if | enemy.y > state.hero.y -> {newEnemy | y <- enemy.y - 3}
                   | enemy.y < state.hero.y -> {newEnemy | y <- enemy.y + 3}
                   | otherwise -> newEnemy
      Spawner -> newEnemy
      Spinner -> {newEnemy | x <- enemy.x - 1 + 12 * cos(enemy.angle), y <- enemy.y + 12 * sin(enemy.angle), angle <- enemy.angle + 0.1}

enemyInDimensions : (Float, Float) -> Enemy -> Bool
enemyInDimensions (windowX, windowY) enemy =
  enemy.x >= (-windowX / 2) - 10

updateHeroBullets : Input -> GameState -> List Bullet
updateHeroBullets inputs state =
  let newBullets =
                    if (not (delayed state.hero.fireDelay) && inputs.fire)
                      then state.heroBullets ++ (fireBullet state.hero.x state.hero.y state.hero.gunType)
                      else state.heroBullets
      nonSpawnerBullets = List.filter (\b -> not (b.bulletType == SpawnerBullet)) newBullets
      spawnerBullets = List.filter (\b -> b.bulletType == SpawnerBullet) newBullets |> List.map spawnerBulletChildren |> List.concat
      newTotalBullets = nonSpawnerBullets ++ spawnerBullets
      dimensions = (toFloat (fst state.dimensions), toFloat (snd state.dimensions))
  in
    List.filter expiredBomb <| List.filter (bulletInDimensions dimensions)
                <| (List.map moveBullet newTotalBullets |> List.map tickUpdate)

expiredBomb : Bullet -> Bool
expiredBomb candidate =
  (not (candidate.bulletType == Bomb)) || candidate.bulletType == Bomb && candidate.tick > -100

spawnerBulletChildren : Bullet -> List Bullet
spawnerBulletChildren spawner =
  let angleDif = 3.14/6.0
  in
    if (spawner.tick <= 0) then [(bullet spawner.x spawner.y spawner.angle SpawnerBullet),(bullet spawner.x spawner.y (spawner.angle + angleDif) SpawnerBullet), (bullet spawner.x spawner.y (spawner.angle - angleDif) SpawnerBullet)]
      else [spawner]

bulletInDimensions : (Float, Float) -> Bullet -> Bool
bulletInDimensions (windowX, windowY) bullet =
  let x = bullet.x
      y = bullet.y
  in
    (x >= (-windowX / 2) - 10  && x <= (windowX / 2) + 10) && (y >= (-windowY / 2) - 10 && y <= (windowY / 2) + 10)

fireBullet : Float -> Float -> GunType -> List Bullet
fireBullet x y gType =
  case gType of
    StandardGun -> [(bullet x y 0.0 StandardBullet)]
    BombLauncher -> [(bullet x y 0.0 Bomb)]
    WShot -> [(bullet x y 0.0 StandardBullet), (bullet x y (3.14/6.0) StandardBullet), (bullet x y (-3.14/6.0) StandardBullet)]
    Beam -> [(bullet x y 0.0 StandardBullet)]
    SpawnerLauncher -> [(bullet x y 0.0 SpawnerBullet)]

tickUpdate : Bullet -> Bullet
tickUpdate bullet =
  {bullet | tick <- bullet.tick - 1}

moveBullet : Bullet -> Bullet
moveBullet bullet =
  let newBullet = {bullet | x <- bullet.x+(cos bullet.angle)* 10.0, y<- bullet.y+(sin bullet.angle)*10.0}
  in
    if (bullet.bulletType == Bomb && bullet.tick < 0) then
      bullet
    else
      newBullet

updateEnemyBullets : GameState -> List Bullet
updateEnemyBullets state =
  let enemiesShooting = List.filter (\e -> e.fireDelay <= 0) state.enemies
      newBullets = List.concat (List.map fireEnemyBullet enemiesShooting)
      mergedBullets = state.enemyBullets ++ newBullets
      nonSpawnerBullets = List.filter (\b -> not (b.bulletType == SpawnerBullet)) mergedBullets
      spawnerBullets = List.filter (\b -> b.bulletType == SpawnerBullet) mergedBullets |> List.map spawnerBulletChildren |> List.concat
      newTotalBullets = nonSpawnerBullets ++ spawnerBullets
      dimensions = (toFloat (fst state.dimensions), toFloat (snd state.dimensions))
  in List.filter (bulletInDimensions dimensions)
              <| (List.map moveBullet newTotalBullets |> List.map tickUpdate)

fireEnemyBullet : Enemy -> List Bullet
fireEnemyBullet enemy =
  case enemy.enemyType of
    Fighter -> [(bullet enemy.x enemy.y 3.14 StandardBullet)]
    Diver   -> []
    Drifter -> [(bullet enemy.x enemy.y 3.14 StandardBullet)]
    Hunter  -> [(bullet enemy.x (enemy.y + 5) 3.14 StandardBullet), (bullet enemy.x (enemy.y - 5) 3.14 StandardBullet)]
    Spawner -> [(bullet enemy.x enemy.y 3.14 SpawnerBullet)]
    Spinner -> [(bullet enemy.x enemy.y 3.14 StandardBullet),
                (bullet enemy.x enemy.y (3.14*(1.0-1.0/6.0)) StandardBullet),
                (bullet enemy.x enemy.y (3.14*(1.0+1.0/6.0)) StandardBullet),
                (bullet enemy.x enemy.y (3.14*(1.0+1.0/3.0)) StandardBullet),
                (bullet enemy.x enemy.y (3.14*(1.0-1.0/3.0)) StandardBullet),
                (bullet enemy.x enemy.y (3.14*(1.0+2.0/3.0)) StandardBullet),
                (bullet enemy.x enemy.y (3.14*(1.0-2.0/3.0)) StandardBullet),
                (bullet enemy.x enemy.y (3.14*(1.0+5.0/6.0)) StandardBullet),
                (bullet enemy.x enemy.y (3.14*(1.0-5.0/6.0)) StandardBullet),
                (bullet enemy.x enemy.y (-3.14/2.0) StandardBullet),
                (bullet enemy.x enemy.y (3.14/2.0) StandardBullet),
                (bullet enemy.x enemy.y 0.0 StandardBullet)]

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

drawBulletStandard : Form
drawBulletStandard =
  filled yellow (circle 5)

drawBulletBomb : Form
drawBulletBomb =
  let smallCircle = filled red (circle 3)
      mediumCircle = filled yellow (circle 5)
      largeCircle = filled red (circle 7)
  in group [largeCircle, mediumCircle, smallCircle]

drawBoom : Form
drawBoom =
  let leBoom = drawBulletBomb
      offset = 7.0/sqrt(2)
  in group [ move (0.0,2.0*offset) leBoom,
             move (2.0*offset,0.0) leBoom,
             move (0.0,-2.0*offset) leBoom,
             move (-2.0*offset,0.0) leBoom,
             move (2.0*offset,2.0*offset) leBoom,
             move (-2.0*offset,2.0*offset) leBoom,
             move (2.0*offset,-2.0*offset) leBoom,
             move (-2.0*offset,-2.0*offset) leBoom,
             move (offset,offset) leBoom,
             move (-offset,offset) leBoom,
             move (offset,-offset) leBoom,
             move (-offset,-offset) leBoom,
             leBoom]

drawBulletSpawner : Form
drawBulletSpawner =
  filled green (circle 5)

-- Code for Test Drawing a single ship onto the board at x,y coords
-- Starfield gif can be obtained at http://30000fps.com/post/93334443098
viewGameState : GameState -> Element
viewGameState state =
  let heroForm = viewHero state.hero
      enemyForms = List.map viewEnemy state.enemies
      bulletForms = List.map viewBullet state.heroBullets
      enemyBulletForms = List.map viewBullet state.enemyBullets
      (w, h) = state.dimensions
      merged = collage w h (bulletForms ++ enemyBulletForms ++ [heroForm] ++ enemyForms)
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

viewBullet : Bullet -> Form
viewBullet bullet =
  let form = case bullet.bulletType of
                StandardBullet -> drawBulletStandard
                Bomb -> if (bullet.tick < 0) then drawBoom else drawBulletBomb
                SpawnerBullet -> if (bullet.tick % 2 == 0) then drawBulletStandard else drawBulletSpawner
 in form |> move (bullet.x, bullet.y)

viewShip : (Float,Float) -> (Int, Int) -> Form -> Element
viewShip (x,y) (w,h) ship =
  let shipImage =
    ship
    |> rotate 3.14
    |> move (x,y)
  in
    collage w h [shipImage]

main =
  map viewGameState updateGameState
