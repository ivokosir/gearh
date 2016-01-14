module Main where

import Graphics.Cogh

import FRP.Gearh

renderInput
  :: Window
  -> (s -> Element)
  -> Input (s -> Output s)
renderInput window getElement =
  allways $ return $ \ state -> action $ renderRoot window $ getElement state

eventInput :: Window -> Input Event
eventInput = Input . getEvents

data State = State
  { x :: Int
  , y :: Int
  , clicks :: Int
  } deriving (Show)

updateEvent :: Event -> State -> Output State
updateEvent Quit _ = finish
updateEvent (MouseButton _ True _) state =
  updateAndAction newState (print newState)
 where
  newState = state { clicks = clicks state +1 }
updateEvent (MousePosition mx my) state  = update newState
 where
  newState = state { x = mx, y = my }
updateEvent _ _  = continue

render :: State -> Element
render state = renderScene (x state) (y state)

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
      [ fmap updateEvent (eventInput window)
      , renderInput window render
      ]

  clicksReturn <- fmap clicks $ runGear input initialState
  putStrLn ("return value: " ++ show clicksReturn)
