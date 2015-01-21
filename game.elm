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
  in rotate 3.14 (group [move (-15,10) triangle
                        ,move (-15,-10) triangle
                        ,mainBody
                        ])

-- Code for Test Drawing a single ship onto the board at x,y coords
viewShip : (Float,Float) -> (Int, Int)  -> Element
viewShip (x,y) (w,h) =
  let shipImage =
    drawHunter
    |> rotate 3.14
    |> move (x,y)
  in
    collage w h [shipImage]

viewShipHero : (Float,Float) -> (Int, Int)  -> Element
viewShipHero (x,y) (w,h) =
  let shipImage =
    drawHero
    |> rotate 3.14
    |> move (x,y)
  in
    collage w h [shipImage]

-- Starfield gif can be obtained at http://30000fps.com/post/93334443098
view : (Int, Int) -> Element
view (w, h) =
  layers [fittedImage w h "/starfield.gif", viewShip (((toFloat w)/2.0-40),0) (w,h), viewShipHero (((toFloat -w)/2.0+40),0) (w,h)]

main =
  map view Window.dimensions
