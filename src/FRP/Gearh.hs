module FRP.Gearh
  ( GearInput(..)
  , merge
  , allways
  , sometimes
  , addIO
  , GearOutput(..)
  , runGear
  ) where

import Control.Monad
import Data.Maybe

newtype GearInput input = GearInput (IO [input])

instance Functor GearInput where
  fmap f (GearInput i) = GearInput $ (fmap . fmap) f i

merge :: [GearInput a] -> GearInput a
merge inputs = GearInput $
  fmap concat $ sequence $ fmap (\ (GearInput i) -> i) inputs

allways :: IO a -> GearInput a
allways io = GearInput $ fmap (:[]) io

sometimes :: IO (Maybe a) -> GearInput a
sometimes io = GearInput $ fmap maybeToList io

addIO :: (a -> IO b) -> GearInput a -> GearInput b
addIO io (GearInput i) = GearInput $ i >>= mapM io

data GearOutput state result
  = GearAction (IO ())
  | GearUpdate state
  | GearUpdateAction state (IO ())
  | GearFinish result

runGear
  :: GearInput input
  -> state
  -> (state -> input -> GearOutput state result)
  -> IO result
runGear (GearInput isIo) initialState f =
  run initialState
 where
  inputLoop (Right oldState) input =
    case f oldState input of
      (GearAction action) -> do
        action
        return (Right oldState)
      (GearUpdate newState) -> return (Right newState)
      (GearUpdateAction newState action) -> do
        action
        return (Right newState)
      (GearFinish result) -> return (Left result)
  inputLoop (Left result) _ = return (Left result)

  run oldState = do
    is <- isIo
    stateOrResult <- foldM inputLoop (Right oldState) is
    case stateOrResult of
      Right newState -> run newState
      Left result -> return result
