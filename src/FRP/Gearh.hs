module FRP.Gearh
  ( GearInput(..)
  , merge
  , allways
  , sometimes
  , addIO
  , GearOutput
  , GearLoop(..)
  , runGear
  ) where

import Control.Monad
import Data.IORef

newtype GearInput a = GearInput (GearOutput a -> IO GearLoop)

type GearOutput a = a -> IO GearLoop

data GearLoop = GearContinue | GearFinish

instance Functor GearInput where
  fmap f (GearInput i) = GearInput $ \ o -> i (o . f)

merge :: [GearInput a] -> GearInput a
merge inputs = GearInput $ \ o ->
  let
    f loop (GearInput i) = case loop of
      GearContinue -> i o
      GearFinish -> return GearFinish
  in foldM f GearContinue inputs

allways :: IO a -> GearInput a
allways io = GearInput $ \ o -> do
  a <- io
  o a

sometimes :: IO (Maybe a) -> GearInput a
sometimes io = GearInput $ \ o -> do
  ma <- io
  case ma of
    Just a -> o a
    Nothing -> return GearContinue

addIO :: (a -> IO b) -> GearInput a -> GearInput b
addIO io (GearInput i) = GearInput $ \ o ->
  i $ \ a -> do
    b <- io a
    o b

runGear
  :: GearInput input
  -> GearOutput output
  -> state
  -> (state -> input -> (state, output))
  -> IO ()
runGear (GearInput i) o initialState f = do
  stateRef <- newIORef initialState

  let
    inputIO input = do
      oldState <- readIORef stateRef
      let (newState, output) = f oldState input
      writeIORef stateRef newState
      o output

    run = do
      loop <- i inputIO
      case loop of
        GearContinue -> run
        GearFinish -> return ()

  run
