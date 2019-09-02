module Handler where

import AWSLambda.Events.APIGateway
  ( APIGatewayProxyRequest
  , APIGatewayProxyResponse(APIGatewayProxyResponse)
  , agprqPath
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
import Data.Aeson (FromJSON, ToJSON, Value, (.=), decode, defaultOptions, genericToEncoding, object, toEncoding)
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Lazy (fromStrict)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (Query, connectPostgreSQL, execute)
import GHC.Generics (Generic)
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
import qualified Network.SendGridV3.Api as SG
import Network.SendGridV3.Api
  ( ApiKey(ApiKey)
  , Mail
  , MailAddress(MailAddress)
  , MailSettings(MailSettings)
  , SandboxMode(SandboxMode)
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
  sendGridApiKey <- fromEnvRequired "FRAUDSTOP_SENDGRID_API_KEY"
  authEmail <- fromEnvRequired "DOCSAWAY_EMAIL"
  key <- fromEnvRequired "DOCSAWAY_KEY"
  stage <- fromEnvOptional "STAGE" "DEV"
  url <- dbUrl
  conn <- connectPostgreSQL url
  -- print request
  let urlPath = BSI.unpackChars $ request ^. agprqPath
  let details = request ^. requestBody
  case (urlPath, details) of
    ("/begin", Just deets) -> do
      _ <- execute conn insertDetails [deets]
      -- print deets
      let sandboxMode =
            if stage == "TEST"
              then Just sandbox
              else Nothing
      statusCode <- SG.sendMail (ApiKey sendGridApiKey) confirmationEmail {SG._mailMailSettings = sandboxMode}
      -- print statusCode
      pure responseOk
    ("/begin", _) -> pure $ response 400
    ("/confirm", Just deets) -> do
      let apiMode =
            if stage == "PROD"
              then "LIVE"
              else "TEST"
      let d = encodeUtf8 deets
      print d
      rsp <- invokeLambda NorthVirginia "fraudstop-dev-letter-func" $ d
      let letter = decode (fromStrict rsp) :: Maybe LambdaResponse
      case letter of
        Just pdf -> do
          result <- sendLetter authEmail key apiMode (body pdf)
          print result
          pure responseOk
        _ -> throw BadLetter
    ("/confirm", _) -> pure $ response 403
    _ -> pure $ response 404

insertDetails :: Query
insertDetails = "insert into user_requests(created_at, details) values(now(), ?)"

confirmationEmail :: Mail () ()
confirmationEmail =
  let to = SG.personalization $ fromList [MailAddress "tim+test@getup.org.au" "John Doe"]
      from = MailAddress "jane@example.com" "Jane Smith"
      subject = "Email Subject"
      content = Just $ fromList [SG.mailContentText "Example Content"]
   in SG.mail [to] from subject content

sandbox :: MailSettings
sandbox = MailSettings Nothing Nothing Nothing (Just (SandboxMode True)) Nothing

newtype LambdaResponse =
  LambdaResponse
    { body :: String
    }
  deriving (Show, Generic)

instance FromJSON LambdaResponse

invokeLambda :: Region -> Text -> BSI.ByteString -> IO BSI.ByteString
invokeLambda region funcName payload = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger lgr
  runResourceT . runAWST env . within region $ do
    rsp <- send $ invoke funcName payload
    case rsp ^. irsPayload of
      Just output -> return output
      Nothing -> throw BadLambdaResponse

sendLetter :: (MonadIO m, ToJSON v1, ToJSON v2, ToJSON v3) => v1 -> v2 -> Text -> v3 -> m Value
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

fromEnvOptional :: String -> String -> IO String
fromEnvOptional var fallback = do
  envVar <- lookupEnv var
  return $ fromMaybe fallback envVar

fromEnvRequired :: String -> IO Text
fromEnvRequired var = do
  envVar <- lookupEnv var
  case envVar of
    Just a -> return $ pack a
    Nothing -> error $ "Please set " <> var

responseOk :: APIGatewayProxyResponse body
responseOk = APIGatewayProxyResponse 200 [] Nothing

response :: Int -> APIGatewayProxyResponse Text
response n = APIGatewayProxyResponse n [] Nothing

dbUrl :: IO BSI.ByteString
dbUrl = do
  envUrl <- lookupEnv "DATABASE_URL"
  return $ BSI.packChars $ fromMaybe "postgresql://localhost/fraudstop" envUrl
