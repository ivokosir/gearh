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

newtype GearInput input = GearInput (IO [IO input])

type GearOutput input result = input -> IO (GearLoop result)

data GearLoop result = GearContinue | GearFinish result

instance Functor GearInput where
  fmap f (GearInput i) = GearInput ((fmap . fmap . fmap) f i)

merge :: [GearInput a] -> GearInput a
merge inputs =
  GearInput $ fmap concat $ sequence $ fmap (\ (GearInput i) -> i) inputs

allways :: IO a -> GearInput a
allways io = GearInput $ return [io]

sometimes :: IO (Maybe a) -> GearInput a
sometimes io = GearInput $ do
  ma <- io
  return $ case ma of
    Just a -> [return a]
    Nothing -> []

addIO :: (a -> IO b) -> GearInput a -> GearInput b
addIO io (GearInput i) =
  GearInput (fmap (fmap (>>= io)) i)

runGear
  :: GearInput input
  -> GearOutput output result
  -> state
  -> (state -> input -> (state, output))
  -> IO result
runGear (GearInput isIo) o initialState f =
  run initialState
 where
  inputLoop (oldState, GearContinue) i = do
    input <- i
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
