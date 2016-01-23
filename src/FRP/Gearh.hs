module FRP.Gearh
  ( Input(..)
  , allways
  , sometimes
  , addIO
  , Output(..)
  , runGear
  ) where

import Data.Maybe

newtype Input i = Input (IO [i])

instance Functor Input where
  fmap f (Input i) = Input $ (fmap . fmap) f i

instance Monoid (Input a) where
  mempty = Input $ return []
  mappend (Input i1) (Input i2) = Input $ do
    a1 <- i1
    a2 <- i2
    return $ a1 ++ a2

allways :: IO a -> Input a
allways io = Input $ fmap (:[]) io

sometimes :: IO (Maybe a) -> Input a
sometimes io = Input $ fmap maybeToList io

addIO :: (a -> IO b) -> Input a -> Input b
addIO io (Input i) = Input $ i >>= mapM io

data Output
  = Output (IO ())
  | Quit

instance Monoid Output where
  mempty = Output $ return ()
  mappend (Output oIO1) (Output oIO2) =
    Output $ oIO1 >> oIO2
  mappend o _ = o

runGear
  :: Input (s -> s)
  -> (s -> Output)
  -> s
  -> IO s
runGear (Input isIO) o initialState =
  run initialState
 where
  run oldState = do
    is <- isIO
    let newState = foldl (\ state input -> input state) oldState is

    case o newState of
      Output oIO -> oIO >> run newState
      Quit -> return newState
