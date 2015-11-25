module Main where

--import Control.Concurrent
import Data.IORef
import Data.Word
import Graphics.Cogh

import FRP.Gearh

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
  = Render
  | Update
  | Log String
  | Exit

update :: Int -> Input -> (Int, Result)
update state (EventInput Quit) = (state, Exit)
update state (EventInput (MouseButton _ True _)) = (state+1, Log (show state))
update state (EventInput _) = (state, Update)
update state (Delta _) = (state, Render)

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
        Render -> do
          clear (window cogh)
          swapBuffers (window cogh)
        Update -> return ()
        Log s -> putStrLn s
        Exit -> deleteWindow (window cogh)

      return $ case result of
        Exit -> GearFinish
        _ -> GearContinue

  runGear input output initialState update
