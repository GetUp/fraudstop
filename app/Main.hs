{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import GHC.Generics

import AWSLambda
import qualified Data.Aeson as Aeson
import Network.AWS.Data
import Network.AWS.Lambda.Invoke

-- don't know which I need :/
import Control.Lens

-- import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS

-- import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BS

-- import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO

-- import Lib
data Person =
  Person
    { name :: Text
    , age :: Int
    }
  deriving (Generic, Show)

instance Aeson.ToJSON Person where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

main :: IO ()
main = lambdaMain handler

event :: BS.ByteString
event = Aeson.encode (Person {name = Text.pack "Joe", age = 12})

handler :: Aeson.Value -> IO [Int]
handler evt = do
  putStrLn "This should go to logs"
  -- someFunc
  invokeLambda NorthVirginia (Text.pack "fraudstop-dev-letter-func") event
  print evt
  pure [1, 2, 3]

-- "fraudstop-dev-letter-func"
invokeLambda :: Region -> Text -> BS.ByteString -> IO ()
invokeLambda region func payload = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger lgr
  let say = liftIO . Text.putStrLn
  runResourceT . runAWST env . within region $ do
    rsp <- send $ invoke func $ toBS payload
    let Just output = rsp ^. irsPayload
    say $ toText output
    -- say $ "Response: " <> toText (view irsPayload <$> rsp)
