module Handler where

import AWSLambda.Events.APIGateway
  ( APIGatewayProxyRequest
  , APIGatewayProxyResponse(APIGatewayProxyResponse)
  , agprqHeaders
  , agprqPath
  , agprqRequestContext
  , prcStage
  , requestBody
  )
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
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (FromRow, Query, connectPostgreSQL, execute)
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Network.AWS.Data (toBS)
import Network.AWS.Lambda.Invoke (invoke, irsPayload)
import Network.HTTP.Req
  ( POST(POST)
  , ReqBodyJson(ReqBodyJson)
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
  deriving (Show, Typeable, Generic)

instance ToJSON Details where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Details

data CustomException
  = BadLambdaResponse
  | BadLetter
  deriving (Show, Typeable)

instance Exception CustomException

handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
  let appUrl = buildAppUrl request
  -- print request
  let urlPath = BSI.unpackChars $ request ^. agprqPath
  authEmail <- fromEnv "DOCSAWAY_EMAIL" ""
  key <- fromEnv "DOCSAWAY_KEY" ""
  apiMode <- fromEnv "API_MODE" "TEST"
  url <- dbUrl
  conn <- connectPostgreSQL url
  let details = request ^. requestBody
  case (urlPath, details) of
    ("/begin", Just deets) -> do
      print deets
      _ <- execute conn insertDetails [deets]
      pure responseOk
    ("/confirm", _) -> pure response403
    -- ("/confirm", _) -> do
    --   rsp <- invokeLambda NorthVirginia "fraudstop-dev-letter-func" event
    --   let letter = decode (fromStrict rsp) :: Maybe LambdaResponse
    --   case letter of
    --     Just pdf -> do
    --       result <- sendLetter authEmail key apiMode (body pdf)
    --       print result
    --       pure responseOk
    --     _ -> throw BadLetter
    _ -> pure response404

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

response404 :: APIGatewayProxyResponse Text
response404 = APIGatewayProxyResponse 404 [] Nothing

response403 :: APIGatewayProxyResponse Text
response403 = APIGatewayProxyResponse 403 [] Nothing

dbUrl :: IO BSI.ByteString
dbUrl = do
  envUrl <- lookupEnv "DATABASE_URL"
  return $ BSI.packChars $ fromMaybe "postgresql://localhost/fraudstop" envUrl

insertDetails :: Query
insertDetails = "insert into user_requests(created_at, details) values(now(), ?)"

wrap :: BSI.ByteString -> Text
wrap = pack . BSI.unpackChars

buildAppUrl :: APIGatewayProxyRequest Text -> Text -> Text
buildAppUrl request path = do
  let headers = request ^. agprqHeaders
  case lookup "Host" headers of
    Nothing -> error "Hostname not found"
    Just host -> do
      let stage = request ^. agprqRequestContext . prcStage
      "https://" <> wrap host <> "/" <> stage <> path
