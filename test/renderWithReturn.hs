module Main where

import Graphics.Cogh

import FRP.Gearh


quitOutput :: Bool -> Output
quitOutput True = quit
quitOutput False = mempty

renderOutput :: Window -> Element -> Output
renderOutput window =
  output . renderRoot window

eventInput :: Window -> Input Event
eventInput = manytimes . getEvents

data State = State
  { x :: Int
  , y :: Int
  , clicks :: Int
  , finish :: Bool
  } deriving (Show)

updateEvent :: Event -> State -> State
updateEvent Quit state = newState
 where
  newState = state { finish = True }
updateEvent (MouseButton _ True _) state = newState
 where
  newState = state { clicks = clicks state +1 }
updateEvent (MousePosition mx my) state = newState
 where
  newState = state { x = mx, y = my }
updateEvent _ state = state

renderScene :: State -> Element
renderScene state =
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
  centerX = x state
  centerY = y state

initialState :: State
initialState = State 0 0 0 False

main :: IO ()
main = do
  (Just window) <- newWindow "Test"

  let
    i = fmap updateEvent (eventInput window)

    o = mconcat
      [ quitOutput . finish
      , renderOutput window . renderScene
      ]

  clicksReturn <- clicks <$> runGear i o initialState
  putStrLn ("return value: " ++ show clicksReturn)
