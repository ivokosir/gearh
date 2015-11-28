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

update :: Int -> Input -> (Int, Result)
update state (EventInput Quit) = (state, Exit state)
update state (EventInput (MouseButton _ True _)) = (state+1, Log (show state))
update state (EventInput _) = (state, Update)
update state (Delta _) = (state, Render renderScene)

renderScene :: Element
renderScene =
  move 100 100 $ group
    [ rectangle 200 200 0xFF0000FF
    , move 200 200 $ rectangle 200 200 0x00FF00FF
    , move 90 210 $ rectangle 100 100 0x0000FFFF
    , move 100 100 $ rectangle 200 200 0xFFFFFF44
    ]

initialState :: Int
initialState = 0

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
        Exit clicks -> GearFinish clicks
        _ -> GearContinue

  clicks <- runGear input output initialState update
  print clicks
