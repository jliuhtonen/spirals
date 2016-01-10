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
import Math (pi, sqrt)
import Prelude

strokeColor = "#6776e6"

main = do
  let arcs = reverse $ calculateArcs {x: 0.0, y: 0.0} 14
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas >>= setStrokeStyle strokeColor
  dimensions <- getCanvasDimensions canvas
  let middle = { x: dimensions.width / 2.0, y: dimensions.height / 2.0 }
  translate { translateX: middle.x, translateY: middle.y } ctx
  requestAnimationFrame (drawFrame canvas ctx arcs)

drawFrame :: forall e. CanvasElement -> Context2D -> List Arc -> Eff (dom :: DOM, canvas :: Canvas | e) Unit
drawFrame canvas ctx arcs = do
  dimensions <- getCanvasDimensions canvas
  clearRect ctx { x: -0.5 * dimensions.width, y: -0.5 * dimensions.height, w: dimensions.width, h: dimensions.height }
  drawArcs arcs ctx
  rotate (-1.0 / (2.0 * pi)) ctx
  requestAnimationFrame (drawFrame canvas ctx arcs)

drawArcs :: forall e. List Arc -> Context2D -> Eff (canvas :: Canvas | e) Unit
drawArcs arcs ctx = traverse_ drawArc arcs where
  drawArc a = do
    beginPath ctx
    arc ctx a
    stroke ctx

calculateArcs :: { x :: Number, y :: Number } -> Int -> List Arc
calculateArcs coords n = calculateArcs' n 0 coords 1 Nil

calculateArcs' :: Int -> Int -> { x :: Number, y :: Number } -> Int -> List Arc -> List Arc
calculateArcs' 0 _ _ _ lst = lst
calculateArcs' n i {x: x, y: y} dir lst = calculateArcs' (n - 1) (i + 1) newCoords newDir (item : lst) where
  r = fibNum i
  newDir = if dir < 4 then dir + 1 else 1
  item = case dir of
              1 -> { x: x, y: y, r: r, start: 0.0, end: pi / 2.0}
              2 -> { x: x, y: y, r: r, start: pi / 2.0, end: pi }
              3 -> { x: x, y: y, r: r, start: pi, end: 3.0 * pi / 2.0 }
              4 -> { x: x, y: y, r: r, start: 3.0 * pi / 2.0, end: 2.0 * pi }
  newCoords = case dir of
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
