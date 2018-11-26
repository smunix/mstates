module Main where

import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad.State.Class (MonadState, get, put)
import           Control.Monad.Trans.State (StateT, runStateT)
import           Data.String.Strip
import qualified Data.Text                 as T

data Value where
  IVal :: Int -> Value
  DVal :: Double -> Value
  TVal :: T.Text -> Value
  deriving (Show)

data ValueError where
  WrongValue :: Value -> T.Text -> ValueError
  deriving (Show)

data ListError where
  UnexpectedEndOfList :: ListError
  UnexpectedValue :: ValueError -> T.Text -> ListError
  ExpectedEoL :: String -> ListError
  deriving (Show)

type Values = [Value]

newtype Decoder a = Decoder { runDecoder :: StateT Values (Either ListError) a }
  deriving (Functor, Applicative, Monad, MonadState Values, MonadError ListError)

value :: (Value -> Either ValueError a) -> Decoder a
value dec = do
  vals <- get
  case vals of
    [] -> throwError UnexpectedEndOfList
    v:vxs -> case dec v of
      Left err@(WrongValue v t) -> throwError (UnexpectedValue err (T.pack $ "`" ++ show err ++ "`"))
      Right a -> do
        put vxs
        return a

data User where
  User :: { name :: T.Text
          , age :: Int
          , height :: Double
          } -> User
  deriving (Show)

user :: Decoder User
user = User <$> text <*> int <*> double
  where
    text :: Decoder T.Text
    text = value (\case
                     TVal i -> Right i
                     v -> Left (WrongValue v "Text expected")
                 )

    int :: Decoder Int
    int = value (\case
                     IVal i -> Right i
                     v -> Left (WrongValue v "Int expected")
                 )

    double :: Decoder Double
    double = value (\case
                     DVal i -> Right i
                     v -> Left (WrongValue v "Double expected")
                 )

decValues :: Values -> Decoder a -> Either ListError a
decValues vxs dec = do
  (a, s) <- runStateT (runDecoder dec) vxs
  case s of
    [] -> return a
    vals -> Left (ExpectedEoL . show $ vals)

main :: IO ()
main = do
  let
    akt :: Either ListError User
    akt = decValues [TVal "Providence", IVal 36, DVal 1.83] user

  putStrLn . show $ akt
