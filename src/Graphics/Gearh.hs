module Graphics.Gearh
  ( Element(..)
  , rectangle
  , image
  , group
  , move
  , renderRoot
  ) where

import Data.Word

import Graphics.Cogh

type X = Int
type Y = Int
type W = Int
type H = Int

type Color = Word32

data Matrix = Matrix X Y

-- multiply :: Matrix -> Matrix -> Matrix
-- multiply (Matrix x1 y1) (Matrix x2 y2) =
--   (Matrix (x1 + x2) (y1 + y2))

type Render = Matrix -> Window -> IO ()

newtype Element = Element Render

rectangle :: W -> H -> Color -> Element
rectangle w h color = Element render
 where
  render (Matrix x y) window =
    drawRect window x y w h color

image :: W -> H -> Texture -> Element
image w h t = Element render
 where
  render (Matrix x y) window =
    drawTexture window t x y w h 0 0 0

group :: [Element] -> Element
group es = Element render
 where
  render matrix window =
    sequence_ (fmap renderElement es)
   where
    renderElement (Element r) = r matrix window

move :: X -> Y -> Element -> Element
move dx dy (Element r) = Element render
 where
  render (Matrix x y) =
    r (Matrix (x + dx) (y + dy))

renderRoot :: Window -> Element -> IO ()
renderRoot window (Element r) = r (Matrix 0 0) window
