module FRP.Gearh
  ( Input(..)
  , merge
  , allways
  , sometimes
  , addIO
  , Output
  , update
  , action
  , updateAndAction
  , continue
  , finish
  , runGear
  ) where

import Control.Monad
import Data.Maybe

newtype Input i = Input (IO [i])

instance Functor Input where
  fmap f (Input i) = Input $ (fmap . fmap) f i

merge :: [Input a] -> Input a
merge inputs = Input $
  fmap concat $ sequence $ fmap (\ (Input i) -> i) inputs

allways :: IO a -> Input a
allways io = Input $ fmap (:[]) io

sometimes :: IO (Maybe a) -> Input a
sometimes io = Input $ fmap maybeToList io

addIO :: (a -> IO b) -> Input a -> Input b
addIO io (Input i) = Input $ i >>= mapM io

data Output s
  = Continue (Maybe s) (Maybe (IO ()))
  | Finish

update :: s -> Output s
update state = Continue (Just state) Nothing

action :: IO () -> Output s
action io = Continue Nothing (Just io)

updateAndAction :: s -> IO () -> Output s
updateAndAction state io = Continue (Just state) (Just io)

continue :: Output s
continue = Continue Nothing Nothing

finish :: Output s
finish = Finish

runGear
  :: Input (s -> Output s)
  -> s
  -> IO s
runGear (Input isIO) initialState =
  run initialState
 where
  inputLoop (Right oldState) input =
    case input oldState of
      (Continue mState mIO) -> do
        fromMaybe (return ()) mIO
        return $ Right $ fromMaybe oldState mState
      (Finish) -> return (Left oldState)
  inputLoop (Left finalState) _ = return (Left finalState)

  run oldState = do
    is <- isIO
    stateOrResult <- foldM inputLoop (Right oldState) is
    case stateOrResult of
      Right newState -> run newState
      Left finalState -> return finalState
