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

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  setFillStyle "#000000" ctx
  dimensions <- getCanvasDimensions canvas
  let middle = { x: dimensions.width / 2.0, y: dimensions.height / 2.0 }
  translate { translateX: middle.x, translateY: middle.y } ctx
  requestAnimationFrame (drawFrame canvas ctx)
  log "Hello sailor!"

drawFrame :: forall e. CanvasElement -> Context2D -> Eff (dom :: DOM, canvas :: Canvas | e) Unit
drawFrame canvas ctx = do
  dimensions <- getCanvasDimensions canvas
  clearRect ctx { x: -0.5 * dimensions.width, y: -0.5 * dimensions.height, w: dimensions.width, h: dimensions.height }
  beginPath ctx
  let arcs = reverse $ calculateArcs {x: 0.0, y: 0.0} 13
  traverse_ (\a -> arc ctx a) arcs
  stroke ctx
  rotate (-1.0 / (2.0 * pi)) ctx
  requestAnimationFrame (drawFrame canvas ctx)

calculateArcs :: { x :: Number, y :: Number } -> Int -> List Arc
calculateArcs coords n = asArc' n 0 coords 1 Nil

asArc' :: Int -> Int -> { x :: Number, y :: Number } -> Int -> List Arc -> List Arc
asArc' 0 _ _  _ lst   = lst
asArc' n i  {x: x, y: y} dir lst = asArc' (n - 1) (i + 1) newCoords newDir (item : lst) where
  r = fibNum i
  newDir = if dir < 4 then dir + 1 else 1
  item = case dir of
              1 -> { x: x, y: y, r: r, start: 0.0, end: pi / 2.0}
              2 -> { x: x, y: y, r: r, start: pi / 2.0, end: pi }
              3 -> { x: x, y: y, r: r, start: pi, end: 3.0 * pi / 2.0 }
              4 -> { x: x, y: y, r: r, start: 3.0 * pi / 2.0, end: 2.0 * pi }
  rFix = 3.0 * r / 2.0
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
