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

newtype GearInput a = GearInput (IO [IO a])

type GearOutput a = a -> IO GearLoop

data GearLoop = GearContinue | GearFinish

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
  -> GearOutput output
  -> state
  -> (state -> input -> (state, output))
  -> IO ()
runGear (GearInput isIo) o initialState f =
  run initialState
 where
  inputLoop (oldState, GearContinue) i = do
    input <- i
    let (newState, output) = f oldState input
    loop <- o output
    return (newState, loop)
  inputLoop (oldState, GearFinish) _ = return (oldState, GearFinish)

  run oldState = do
    is <- isIo
    (newState, loop) <- foldM inputLoop (oldState, GearContinue) is
    case loop of
      GearContinue -> run newState
      GearFinish -> return ()
