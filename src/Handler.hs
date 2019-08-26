module Handler
  ( handler
  ) where

import AWSLambda.Events.APIGateway (APIGatewayProxyRequest, APIGatewayProxyResponse(..))
import Control.Exception (Exception, throw)
import Control.Lens ((<&>), (^.), set)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.AWS
  ( Credentials(Discover)
  , LogLevel(Debug)
  , Region(NorthVirginia)
  , envLogger
  , newEnv
  , newLogger
  , runAWST
  , runResourceT
  , send
  , within
  )
import Data.Aeson (FromJSON, ToJSON, Value, (.=), decode, defaultOptions, encode, genericToEncoding, object, toEncoding)
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.AWS.Data (toBS)
import Network.AWS.Lambda.Invoke (invoke, irsPayload)
import Network.HTTP.Req
  ( POST(..)
  , ReqBodyJson(..)
  , (/:)
  , defaultHttpConfig
  , https
  , jsonResponse
  , req
  , responseBody
  , runReq
  )
import System.Environment (lookupEnv)
import System.IO (stdout)

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
  deriving (Show, Generic)

data CustomException
  = BadLambdaResponse
  | BadLetter
  deriving (Show, Typeable)

instance Exception CustomException

instance ToJSON Details where
  toEncoding = genericToEncoding defaultOptions

event :: ByteString
event =
  encode
    (Details
       { firstName = "alice"
       , lastName = "citizen"
       , email = "tim+alicecitizen@mcewan.it"
       , address = "7 henry dr"
       , suburb = "picnic point"
       , postcode = "2341"
       , dob = "01/01/1900"
       , phone = "0123456789"
       , crn = "123456789x"
       , debtReason = "Lorem ipsum dolor sit amet."
       , personalCircumstances = ["Addiction", "sfdg sdfgsdfg dsfg"]
       })

handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
  print request
  authEmail <- fromEnv "DOCSAWAY_EMAIL" ""
  key <- fromEnv "DOCSAWAY_KEY" ""
  apiMode <- fromEnv "API_MODE" "TEST"
  rsp <- invokeLambda NorthVirginia "fraudstop-dev-letter-func" event
  let letter = decode (fromStrict rsp) :: Maybe LambdaResponse
  case letter of
    Just pdf -> do
      result <- sendLetter authEmail key apiMode (body pdf)
      print result
    _ -> throw BadLetter
  pure responseOk

newtype LambdaResponse =
  LambdaResponse
    { body :: String
    }
  deriving (Show, Generic)

instance FromJSON LambdaResponse

invokeLambda :: Region -> Text -> ByteString -> IO BSI.ByteString
invokeLambda region funcName payload = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger lgr
  runResourceT . runAWST env . within region $ do
    rsp <- send $ invoke funcName $ toBS payload
    case rsp ^. irsPayload of
      Just output -> return output
      Nothing -> throw BadLambdaResponse

sendLetter :: (MonadIO m, ToJSON v1, ToJSON v2, ToJSON v3, ToJSON v4) => v1 -> v2 -> v3 -> v4 -> m Value
sendLetter authEmail key apiMode letter =
  runReq defaultHttpConfig $ do
    let endpoint = https "www.docsaway.com" /: "app" /: "api" /: "rest" /: "mail.json"
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
    r <- req POST endpoint (ReqBodyJson payload) jsonResponse mempty
    return (responseBody r :: Value)

fromEnv :: String -> String -> IO String
fromEnv var fallback = do
  envVar <- lookupEnv var
  return $ fromMaybe fallback envVar

responseOk :: APIGatewayProxyResponse body
responseOk = APIGatewayProxyResponse 200 [] Nothing
-- response404 :: APIGatewayProxyResponse Text
-- response404 = APIGatewayProxyResponse 404 [] Nothing
