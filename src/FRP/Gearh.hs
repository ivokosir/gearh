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
import Data.Maybe

newtype GearInput input = GearInput (IO [input])

type GearOutput input result = input -> IO (GearLoop result)

data GearLoop result = GearContinue | GearFinish result

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

runGear
  :: GearInput input
  -> GearOutput output result
  -> state
  -> (state -> input -> (state, output))
  -> IO result
runGear (GearInput isIo) o initialState f =
  run initialState
 where
  inputLoop (oldState, GearContinue) input = do
    let (newState, output) = f oldState input
    loop <- o output
    return (newState, loop)
  inputLoop (oldState, finish) _ = return (oldState, finish)

  run oldState = do
    is <- isIo
    (newState, loop) <- foldM inputLoop (oldState, GearContinue) is
    case loop of
      GearContinue -> run newState
      GearFinish result -> return result
