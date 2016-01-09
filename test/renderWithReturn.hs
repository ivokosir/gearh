module Main where

import Data.Word
import Graphics.Cogh

import FRP.Gearh

delta :: Window -> GearInput Window
delta window = allways $ return window

eventInput :: Window -> GearInput Event
eventInput window = GearInput $ getEvents $ window

data Input
  = Delta Window
  | EventInput Event

data State = State
  { x :: Int
  , y :: Int
  , clicks :: Int
  } deriving (Show)

update :: State -> Input -> GearOutput State Int
update state (EventInput Quit) = GearFinish (clicks state)
update state (EventInput (MouseButton _ True _)) =
  GearUpdateAction newState (putStrLn (show newState))
 where
  newState = state { clicks = clicks state +1 }
update state (EventInput (MousePosition mx my)) = GearUpdate newState
 where
  newState = state { x = mx, y = my }
update state (EventInput _) = GearUpdate state
update state (Delta window) = GearAction (renderRoot window (renderScene (x state) (y state)))

renderScene :: Int -> Int -> Element
renderScene centerX centerY =
  move centerX centerY $ group
    [ move (-w2) (-h2) $ rectangle w2 h2 0xFF0000FF
    , rectangle w2 h2 0x00FF00FF
    , move (-110) 10 $ rectangle 100 100 0x0000FFFF
    , move (-w4) (-h4) $ rectangle w2 h2 0xFFFFFF44
    ]
 where
  (w, h) = (400, 400)
  (w2, h2) = (div w 2, div h 2)
  (w4, h4) = (div w 4, div h 4)

initialState :: State
initialState = State 0 0 0

main :: IO ()
main = do
  (Just window) <- newWindow "Test"

  let
    input = merge
      [ fmap EventInput (eventInput window)
      , fmap Delta (delta window)
      ]

  clicksReturn <- runGear input initialState update
  putStrLn ("return value: " ++ show clicksReturn)
