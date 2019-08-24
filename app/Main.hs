{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import AWSLambda
import Control.Exception (Exception, catch, throw)
import Control.Lens ((<&>), (^.), set)
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, (.=), decode, object)

-- import Data.Aeson (Object, (.:), (.=), decode, object)
-- import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BS

-- import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- import qualified Data.Text.IO as Text
import Data.Typeable
import GHC.Generics
import Network.AWS.Data
import Network.AWS.Lambda.Invoke
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req ((/:), defaultHttpConfig, jsonResponse)
import System.Environment (lookupEnv)
import System.IO

-- import Lib
data Details =
  Details
    { firstName :: Text
    , lastName :: Text
    , email :: Text
    , address :: Text
    , suburb :: Text
    , postcode :: Text
    , dob :: Text
    , phone :: Text
    , crn :: Text
    , debtReason :: Text
    , personalCircumstances :: [Text]
    }
  deriving (Generic, Show)

data CustomException =
  BadLetter
  deriving (Show, Typeable)

instance Exception CustomException

-- data APIConnection =
--   APIConnection
--     { email :: Text
--     , key :: Text
--     }
-- data PrintingStation =
--   PrintingStation
--     { id :: Text
--     , courierID :: Bool
--     , ink :: Text
--     , paper :: Text
--     }
-- data LetterRequest =
--   LetterRequest
--   {
--       APIMode :: Text,
--       APIReport: true,
--       PDFFile: encode64(filename),
--       Recipient: {
--         name: 'Centrelink',
--         company: '',
--         address1: 'Reply Paid 7800',
--         address2: '',
--         city: 'Canberra BC',
--         state: 'ACT',
--         zip: '2610',
--         country: 'AU',
--       },
--     }
--   deriving (Generic, Show)
instance Aeson.ToJSON Details where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

main :: IO ()
main = lambdaMain handler `catch` errorHandler
  where
    errorHandler :: CustomException -> IO ()
    errorHandler err = putStrLn (show err)

event :: BS.ByteString
event =
  Aeson.encode
    (Details
       { firstName = Text.pack "alice"
       , lastName = Text.pack "citizen"
       , email = Text.pack "tim+alicecitizen@mcewan.it"
       , address = Text.pack "7 henry dr"
       , suburb = Text.pack "picnic point"
       , postcode = Text.pack "2341"
       , dob = Text.pack "01/01/1900"
       , phone = Text.pack "0123456789"
       , crn = Text.pack "123456789x"
       , debtReason =
           Text.pack
             "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
       , personalCircumstances = [Text.pack "Addiction", Text.pack "sfdg sdfgsdfg dsfg"]
       })

handler :: Aeson.Value -> IO [Int]
handler evt = do
  print evt
  authEmail <- fromEnv "DOCSAWAY_EMAIL" ""
  key <- fromEnv "DOCSAWAY_KEY" ""
  apiMode <- fromEnv "API_MODE" "TEST"
  rsp <- invokeLambda NorthVirginia (Text.pack "fraudstop-dev-letter-func") event
  let letter = decode (BS.fromStrict rsp) :: Maybe LambdaResponse
  case letter of
    Just pdf -> do
      result <- sendLetter authEmail key apiMode (body pdf)
      print result
    _ -> throw BadLetter
  -- result <- sendLetter authEmail key apiMode letter
  -- print result
  pure [1, 2, 3]

data LambdaResponse =
  LambdaResponse
    { body :: String
    }
  deriving (Show, Generic, FromJSON)

-- letterOpener :: BSI.ByteString -> Maybe LambdaResponse
-- letterOpener r =
-- "fraudstop-dev-letter-func"
invokeLambda :: Region -> Text -> BS.ByteString -> IO BSI.ByteString
invokeLambda region funcName payload = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger lgr
  -- let say = liftIO . Text.putStrLn
  runResourceT . runAWST env . within region $ do
    rsp <- send $ invoke funcName $ toBS payload
    let Just output = rsp ^. irsPayload
    return output

-- result <- decode (BS.fromStrict (output :: BSI.ByteString)) :: Maybe Object
-- liftIO $ print result -- flip parseMaybe result $ \obj -> do
-- --   pdf <- obj .: "body"
-- --   liftIO $ print pdf -- return letter -- liftIO $ print pdf
--   -- case pdf of
--   --   Just letter -> return letter
--   --   _ -> throw BadLetter
sendLetter ::
     (Control.Monad.IO.Class.MonadIO m, Aeson.ToJSON v1, Aeson.ToJSON v2, Aeson.ToJSON v3, Aeson.ToJSON v4)
  => v1
  -> v2
  -> v3
  -> v4
  -> m Aeson.Value
sendLetter authEmail key apiMode letter = do
  Req.runReq defaultHttpConfig $ do
    let endpoint = Req.https "www.docsaway.com" /: "app" /: "api" /: "rest" /: "mail.json"
    let apiConnection = object ["email" .= authEmail, "key" .= key]
    let printingStation =
          object ["id" .= ("AUTO" :: Text), "courierID" .= False, "ink" .= ("BW" :: Text), "paper" .= ("80" :: Text)]
    let recipient =
          object
            [ "name" .= ("Centrelink" :: Text)
            , "company" .= ("" :: Text)
            , "address1" .= ("Reply Paid 7800" :: Text)
            , "address2" .= ("" :: Text)
            , "city" .= ("Canberra BC" :: Text)
            , "state" .= ("ACT" :: Text)
            , "zip" .= ("2610" :: Text)
            , "country" .= ("AU" :: Text)
            ]
    let payload =
          object
            [ "APIConnection" .= apiConnection
            , "APIMode" .= apiMode
            , "APIReport" .= True
            , "PDFFile" .= letter
            , "PrintingStation" .= printingStation
            , "Recipient" .= recipient
            ]
    r <- Req.req Req.POST endpoint (Req.ReqBodyJson payload) jsonResponse mempty
    -- liftIO $ print (Req.responseBody r :: Aeson.Value)
    return (Req.responseBody r :: Aeson.Value)

fromEnv :: String -> String -> IO String
fromEnv var fallback = do
  envVar <- lookupEnv var
  return $ fromMaybe fallback envVar
