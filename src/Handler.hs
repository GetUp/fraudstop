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
import Crypto.Hash (SHA256(SHA256), hashWith)
import Data.Aeson (FromJSON, ToJSON, Value, (.=), decode, defaultOptions, encode, genericToEncoding, object, toEncoding)
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import Data.MonoTraversable (replaceElemStrictText)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (Only(Only), Query, connectPostgreSQL, query)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, fromJSONField)
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

instance FromField Details where
  fromField = fromJSONField

detailDecoder :: Text -> Maybe Details
detailDecoder = decode . fromStrict . encodeUtf8

dbEncode :: Details -> BSI.ByteString
dbEncode = toStrict . encode

data Confirmation =
  Confirmation
    { requestId :: Int
    , token :: Text
    }
  deriving (Show, Typeable, Generic)

instance ToJSON Confirmation where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Confirmation

confirmationDecoder :: Text -> Maybe Confirmation
confirmationDecoder = decode . fromStrict . encodeUtf8

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
  case urlPath of
    "/begin" -> do
      let maybeDetails = request ^. requestBody >>= detailDecoder
      case maybeDetails of
        Just details -> do
          [Only requestId] <- query conn insertDetails [encode details]
          let envelope = confirmationEmail stage details requestId
          print envelope
          status <- SG.sendMail (ApiKey sendGridApiKey) envelope {SG._mailMailSettings = sandboxMode stage}
          print status
          pure responseOk
        Nothing -> pure $ response 400
    "/confirm" -> do
      let maybeConfirmation = request ^. requestBody >>= confirmationDecoder
      -- TODO validate token
      case maybeConfirmation of
        Just confirmation -> do
          [Only (maybeDetails :: Maybe Details)] <- query conn maybeAcquireDetails [requestId confirmation]
          case maybeDetails of
            Just details -> do
              let apiMode =
                    if stage == "PROD"
                      then "LIVE"
                      else "TEST"
              rsp <- invokeLambda NorthVirginia "fraudstop-dev-letter-func" $ dbEncode details
              let letter = decode (fromStrict rsp) :: Maybe LambdaResponse
              case letter of
                Just pdf -> do
                  result <- sendLetter authEmail key apiMode (body pdf)
                  print result
                  pure responseOk
                _ -> throw BadLetter
              pure $ response 403
            Nothing -> pure $ response 403
        Nothing -> pure $ response 403
    _ -> pure $ response 404

insertDetails :: Query
insertDetails = "insert into user_requests(created_at, details) values(now(), ?) returning id"

maybeAcquireDetails :: Query
maybeAcquireDetails =
  "update user_requests set locked_at = now() where processed_at is null and locked_at is null and id = ? returning details"

confirmationEmail :: String -> Details -> Int -> Mail () ()
confirmationEmail stage details requestId =
  let sanitisedAddress = replaceElemStrictText '@' '_' $ email details
      emailAddress =
        if stage == "PROD"
          then email details
          else "tech+" <> sanitisedAddress <> "@getup.org.au"
      recipient = MailAddress emailAddress (firstName details <> lastName details)
      to = SG.personalization $ fromList [recipient]
      from = MailAddress "info+fraudstop@getup.org.au" "GetUp"
      subject = "Please confirm your email address"
      content = Just $ fromList [SG.mailContentHtml $ confirmationEmailContent details requestId]
   in SG.mail [to] from subject content

confirmationEmailContent :: Details -> Int -> Text
confirmationEmailContent d requestId =
  let secureToken = tShow $ hashWith SHA256 $ encodeUtf8 $ email d <> "abcdef"
      params = "?request_id=" <> tShow requestId <> "&secure_token=" <> secureToken
      link = "https://raise-newstart.com/fraudstop/confirm" <> params
   in "Dear " <> firstName d <> ",<br><br>Your FraudStop appeal request has been received.<br><br><a href=\"" <> link <>
      "\">Please confirm your email so that your request can be processed.</a><br><br>You will receive a confirmation email when processing is complete. If you do not receive a confirmation email within 24 hours, please call us on (02) 9211 4400.<br><br>You may also receive BCC copies of several other emails, if you chose those options.  These are for your reference only; you do not need to do anything further with them.<br><br>Thank you for using FraudStop."

sandboxMode :: String -> Maybe MailSettings
sandboxMode stage =
  if stage == "TEST"
    then Just (MailSettings Nothing Nothing Nothing (Just (SandboxMode True)) Nothing)
    else Nothing

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

tShow :: Show a => a -> Text
tShow = pack . show
