module Main where

--import Control.Concurrent
import Data.IORef
import Data.Word
import Graphics.Cogh

import FRP.Gearh
import Graphics.Gearh

data CoghWorld = CoghWorld
  { window :: Window
  , rTime :: IORef Word32
  }

coghWorld :: String -> IO (Maybe CoghWorld)
coghWorld title = do
  mw <- newWindow title
  case mw of
    Nothing -> return Nothing
    Just w -> do
      initialTime <- newIORef =<< time
      return (Just (CoghWorld w initialTime))

delta :: CoghWorld -> GearInput Double
delta cogh = allways $ do
  pastTime <- readIORef timeRef
  currentTime <- time
  writeIORef timeRef currentTime
  return (fromIntegral (currentTime - pastTime) * 1.0E-3)
 where
  timeRef = rTime cogh

eventInput :: CoghWorld -> GearInput Event
eventInput cogh = GearInput $ eventInput' []
 where
  eventInput' es = do
    me <- nextEvent (window cogh)
    case me of
      Just e -> eventInput' (return e : es)
      Nothing -> return es

data Input
  = Delta Double
  | EventInput Event

data Result
  = Render Element
  | Update
  | Log String
  | Exit Int

data State = State
  { x :: Int
  , y :: Int
  , clicks :: Int
  } deriving (Show)

update :: State -> Input -> (State, Result)
update state (EventInput Quit) = (state, Exit (clicks state))
update state (EventInput (MouseButton _ True _)) =
  (newState, Log (show newState))
 where
  newState = state { clicks = clicks state +1 }
update state (EventInput (MousePosition mx my)) = (newState, Update)
 where
  newState = state { x = mx, y = my }
update state (EventInput _) = (state, Update)
update state (Delta _) = (state, Render (renderScene (x state) (y state)))

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
  (Just cogh) <- coghWorld "Test"

  let
    input = merge
      [ fmap EventInput (eventInput cogh), fmap Delta (delta cogh) ]

    output result = do

      case result of
        Render root -> do
          clear (window cogh)
          renderRoot (window cogh) root
          swapBuffers (window cogh)
        Update -> return ()
        Log s -> putStrLn s
        Exit _ -> deleteWindow (window cogh)

      return $ case result of
        Exit clicksReturn -> GearFinish clicksReturn
        _ -> GearContinue

  clicksReturn <- runGear input output initialState update
  putStrLn ("return value: " ++ show clicksReturn)
