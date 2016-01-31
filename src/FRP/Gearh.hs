module FRP.Gearh
  ( Input
  , allways
  , sometimes
  , manytimes
  , Output
  , output
  , quit
  , runGear
  , ResourceStatus (..)
  , ResourceRequest (..)
  , Resource (resourceStatus, resourceRequest)
  , createResource
  , withResource
  ) where

import Data.Maybe

import Data.IORef

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

manytimes :: IO [a] -> Input a
manytimes = Input

data Output = Output { runOutput :: IO Bool }

output :: IO () -> Output
output io = Output $ io >> return False

quit :: Output
quit = Output $ return True

instance Monoid Output where
  mempty = Output $ return False
  mappend (Output oIO1) (Output oIO2) = Output $ do
    o1 <- oIO1
    if o1
      then return o1
      else oIO2

runGear
  :: Input (s -> s)
  -> (s -> Output)
  -> s
  -> IO s
runGear (Input isIO) outputF initialState =
  run initialState
 where
  run oldState = do
    is <- isIO
    let newState = foldl (\ state input -> input state) oldState is

    o <- runOutput (outputF newState)
    if o
      then return newState
      else run newState


data ResourceRequest
  = Load
  | Unload

data ResourceStatus
  = Loaded
  -- | Loading
  | Unloaded
  -- | Unloading
  deriving (Eq)

data Resource a = Resource
  { readResource :: IO (Maybe a)
  , resourceRequest :: ResourceRequest -> Output
  , resourceStatus :: Input ResourceStatus
  }

createResource
  :: IO a
  -> (a -> IO ())
  -> IO (Resource a)
createResource load unload = do
  ref <- newIORef Nothing
  statusRef <- newIORef Unloaded

  let
    resOutput req = output $ do
      mA <- readIORef ref

      case (req, mA) of

        (Load, Nothing) -> do
          a <- load
          writeIORef ref (Just a)

        (Unload, Just a) -> do
          unload a
          writeIORef ref Nothing

        _ -> return ()

    resInput = sometimes $ do
      mA <- readIORef ref
      oldStatus <- readIORef statusRef

      let
        newStatus = case mA of
          Just _ -> Loaded
          Nothing -> Unloaded

      if newStatus == oldStatus
        then return Nothing
        else do
          writeIORef statusRef newStatus
          return $ Just newStatus

  return $ Resource (readIORef ref) resOutput resInput

withResource :: Resource a -> (b -> Output) -> (Maybe a -> b) -> Output
withResource res out func = Output $ do
  mA <- readResource res
  runOutput $ out (func mA)
