module Main where

import Data.Int
import Data.Maybe
import Data.List
import Data.Foldable
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Graphics.Canvas
import DOM
import DOM.RequestAnimationFrame
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.DOM.Simple.Events
import Math (pi, sqrt)
import Prelude

strokeColor = "#FFD700"

main = do
  let arcs = reverse $ calculateArcs {x: 0.0, y: 0.0} 14
  Just canvas <- getCanvasElementById "canvas"
  resizeCanvas canvas
  ctx <- getContext2D canvas >>= setStrokeStyle strokeColor >>= setLineWidth 2.0
  dimensions <- getCanvasDimensions canvas
  let middle = { x: dimensions.width / 2.0, y: dimensions.height / 2.0 }
  translate { translateX: middle.x, translateY: middle.y } ctx
  requestAnimationFrame (drawFrame canvas ctx arcs)

resizeCanvas :: forall e. CanvasElement -> Eff (dom :: DOM, canvas :: Canvas | e) Unit
resizeCanvas canvas = do
  w <- innerWidth globalWindow
  h <- innerHeight globalWindow
  setCanvasWidth w canvas
  setCanvasHeight h canvas
  pure unit

drawFrame :: forall e. CanvasElement -> Context2D -> List Arc -> Eff (dom :: DOM, canvas :: Canvas | e) Unit
drawFrame canvas ctx arcs = do
  dimensions <- getCanvasDimensions canvas
  clearRect ctx { x: -0.5 * dimensions.width, y: -0.5 * dimensions.height, w: dimensions.width, h: dimensions.height }
  drawArcs arcs ctx
  rotate (-0.25 / (2.0 * pi)) ctx
  requestAnimationFrame (drawFrame canvas ctx arcs)

drawArcs :: forall e. List Arc -> Context2D -> Eff (canvas :: Canvas | e) Unit
drawArcs arcs ctx = traverse_ drawArc arcs where
  drawArc a = do
    beginPath ctx
    arc ctx a
    stroke ctx

calculateArcs :: { x :: Number, y :: Number } -> Int -> List Arc
calculateArcs coords n = calculateArcs' n coords 1 Nil

calculateArcs' :: Int -> { x :: Number, y :: Number } -> Int -> List Arc -> List Arc
calculateArcs' 0 _ _ lst = lst
calculateArcs' n {x: x, y: y} quadrant lst = calculateArcs' (n - 1) newCoords newQuadrant (item : lst) where
  index = length lst
  r = fibNum index
  newQuadrant = if quadrant < 4 then quadrant + 1 else 1
  item = case quadrant of
    1 -> { x: x, y: y, r: r, start: 0.0, end: pi / 2.0}
    2 -> { x: x, y: y, r: r, start: pi / 2.0, end: pi }
    3 -> { x: x, y: y, r: r, start: pi, end: 3.0 * pi / 2.0 }
    4 -> { x: x, y: y, r: r, start: 3.0 * pi / 2.0, end: 2.0 * pi }
  newCoords = case quadrant of
    1 -> { x: x, y: y - (r / goldenRatio) }
    2 -> { x: x + (r / goldenRatio), y: y }
    3 -> { x: x, y: y + (r / goldenRatio) }
    4 -> { x: x - (r / goldenRatio), y: y }

goldenRatio = (1.0 + (sqrt 5.0)) / 2.0

fibNum = toNumber <<< fib

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = (fib (n - 2)) + (fib (n - 1))
