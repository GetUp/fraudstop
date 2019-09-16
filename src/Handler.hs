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
import qualified Data.ByteString.Lazy.Internal as BSLI
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)
import Data.MonoTraversable (replaceElemStrictText)
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (Only(Only), Query, connectPostgreSQL, query)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, fromJSONField)

-- import Debug.Trace (trace)
import GHC.Generics (Generic)
import Network.AWS.Lambda.Invoke (invoke, irsPayload)
import Network.HTTP.Client (HttpException, Response)
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
  , Disposition(Attachment)
  , Mail
  , MailAddress(MailAddress)
  , MailAttachment(MailAttachment)
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
    , emailMP :: Bool
    , emailMinister :: Bool
    , submitFoi :: Bool
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

data Verification =
  Verification
    { requestId :: Int
    , token :: Text
    }
  deriving (Show, Typeable, Generic)

instance ToJSON Verification where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Verification

verificationDecoder :: Text -> Maybe Verification
verificationDecoder = decode . fromStrict . encodeUtf8

data CustomException
  = BadLambdaResponse
  | BadLetter
  deriving (Show, Typeable)

instance Exception CustomException

handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
  sendGridApiKey <- fromEnvRequired "FRAUDSTOP_SENDGRID_API_KEY"
  docsEmail <- fromEnvRequired "DOCSAWAY_EMAIL"
  docsKey <- fromEnvRequired "DOCSAWAY_KEY"
  lambdaName <- fromEnvOptional "LETTER_LAMBDA" "fraudstop-dev-letter-func"
  stage <- fromEnvOptional "STAGE" "DEV"
  salt <- fromEnvOptional "SALT" "abcdefg"
  url <- dbUrl
  conn <- connectPostgreSQL url
    -- print request
  let securer = secureToken salt
  let mailer = mailSender sendGridApiKey stage
  let addresser = mailAddresser stage
  let urlPath = BSI.unpackChars $ request ^. agprqPath
  case urlPath of
    "/begin" -> do
      let maybeDetails = request ^. requestBody >>= detailDecoder
      case maybeDetails of
        Just details -> do
          [Only requestId] <- query conn insertDetails [encode details]
          let token = securer requestId
          status <- mailer verificationEmail addresser token details requestId
          print status
          pure responseOk
        Nothing -> pure $ response 400
    "/verify" -> do
      let maybeVerification = request ^. requestBody >>= verificationDecoder
      case maybeVerification of
        Just verification -> do
          let requestId' = requestId verification
          [Only (maybeDetails :: Maybe Details)] <- query conn maybeAcquireDetails [requestId']
          case maybeDetails of
            Just details ->
              if securer requestId' == token verification
                then (do rsp <- invokeLambda NorthVirginia lambdaName $ dbEncode details
                         let letter = decode (fromStrict rsp) :: Maybe LambdaResponse
                         case letter of
                           Just pdf -> do
                             result <- sendLetter docsEmail docsKey stage (body pdf)
                             print result
                             confirmationStatus <- mailer $ confirmationEmail addresser details (body pdf)
                             print confirmationStatus
                             foiStatus <- mailer $ foiEmail addresser details
                             print foiStatus
                             pure responseOk
                           _ -> throw BadLetter)
                else pure $ response 403
                -- pure $ response 403
            Nothing -> pure $ response 403
        Nothing -> pure $ response 403
    _ -> pure $ response 404

secureToken :: Text -> Int -> Text
secureToken salt requestId = tShow $ hashWith SHA256 $ encodeUtf8 $ salt <> tShow requestId

mailSender :: (ToJSON a, ToJSON b) => Text -> Text -> Mail a b -> IO (Either HttpException (Response BSLI.ByteString))
mailSender key stage mail = SG.sendMail (ApiKey key) mail {SG._mailMailSettings = sandboxMode stage}

mailAddresser :: (Eq a, IsString a) => a -> Text -> Text -> SG.Personalization
mailAddresser stage email' name =
  let to =
        if stage == "PROD"
          then email'
          else "tim+" <> replaceElemStrictText '@' '_' email' <> "@getup.org.au"
   in SG.personalization $ fromList [MailAddress to name]

-- validateToken :: Text -> Text -> Text -> Bool
-- validateToken salt email token =
--   let realToken =
--    in realToken == token
--   --  in trace (unpack salt) $ trace (unpack email) $ trace (unpack realToken) $ trace (unpack token) $ realToken == token
insertDetails :: Query
insertDetails = "insert into user_requests(created_at, details) values(now(), ?) returning id"

maybeAcquireDetails :: Query
maybeAcquireDetails =
  "update user_requests set locked_at = now() where processed_at is null and locked_at is null and id = ? returning details"

verificationEmail :: (Text -> Text -> SG.Personalization) -> Text -> Details -> Int -> Mail () ()
verificationEmail addresser token details requestId =
  let to = addresser (email details) (firstName details <> " " <> lastName details)
      from = MailAddress "info+fraudstop@getup.org.au" "GetUp"
      subject = "Please verify your email address"
      content = Just $ fromList [SG.mailContentHtml $ verificationEmailContent token details requestId]
   in SG.mail [to] from subject content

verificationEmailContent :: Text -> Details -> Int -> Text
verificationEmailContent token d requestId =
  let params = "?request_id=" <> tShow requestId <> "&secure_token=" <> token
      link = "https://raise-newstart.com/fraudstop/verify" <> params
   in "Dear " <> firstName d <> ",<br><br>Your FraudStop appeal request has been received.<br><br><a href=\"" <> link <>
      "\">Please click here to verify your email address so that your request can be processed.</a><br><br>You will receive a confirmation email when processing is complete. If you do not receive a confirmation email within 24 hours, please call us on (02) 9211 4400.<br><br>You may also receive BCC copies of several other emails, if you chose those options.  These are for your reference only; you do not need to do anything further with them.<br><br>Thank you for using FraudStop."

confirmationEmail :: (Text -> Text -> SG.Personalization) -> Details -> String -> Mail () ()
confirmationEmail addresser details letter =
  let fName = firstName details
      to = addresser (email details) (fName <> " " <> lastName details)
      from = MailAddress "info+fraudstop@getup.org.au" "GetUp"
      subject = "Request processed confirmation"
      content = Just $ fromList [SG.mailContentText $ confirmationEmailContent fName]
      attachment = createAttachment $ pack letter
   in (SG.mail [to] from subject content) {SG._mailAttachments = Just [attachment]}

confirmationEmailContent :: Text -> Text
confirmationEmailContent name =
  "Dear " <> name <>
  ", your request has been processed.  A review letter has been sent to Centrelink on your behalf. A digital copy of the letter has been attached for your reference.\n\nPS You may also receive BCC copies of several other emails, if you chose those options.  These are for your reference only; you do not need to do anything further with them."

createAttachment :: Text -> MailAttachment
createAttachment pdf =
  MailAttachment
    { SG._mailAttachmentContent = pdf
    , SG._mailAttachmentType = Just "application/pdf"
    , SG._mailAttachmentFilename = "review_request.pdf"
    , SG._mailAttachmentDisposition = Just Attachment
    , SG._mailAttachmentContentId = ""
    }

foiEmail :: (Text -> Text -> SG.Personalization) -> Details -> Mail () ()
foiEmail addresser details =
  let to = addresser "freedomofinformation@humanservices.gov.au" "Dept. of Human Services"
      from = MailAddress (email details) (firstName details <> " " <> lastName details)
      subject = "FOI request for Centrelink file"
      content = Just $ fromList [SG.mailContentText $ foiEmailContent details]
   in SG.mail [to] from subject content

foiEmailContent :: Details -> Text
foiEmailContent d =
  let senderName = firstName d <> " " <> lastName d
   in "Dear Department of Human Services,\n\nI am writing to request under the Freedom of Information Act 1982 copies of all Centrelink file papers, computer records and/or printouts concerning me, " <>
      senderName <>
      " (date of birth: " <>
      dob d <>
      ", Centrelink CRN: " <>
      crn d <>
      ").\n\nPlease include all Centrelink files (including any debt files), as well as all file papers, computer records and/or printouts concerning me from the Customer Archive Retrieval System, batch storage, and any relevant captures of Centrelink computer screens concerning me.\n\nI request that, wherever technically possible, you provide these records in their original accessible electronic form, rather than images of the documents such as scans or screenshots.\n\nPlease provide copies of these records by email to " <>
      email d <>
      ".\n\nI look forward to receiving your acknowledgement of receipt of this request within 14 days, and your reply within 30 days.\n\nBest regards\n\n" <>
      senderName

sandboxMode :: Text -> Maybe MailSettings
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
sendLetter docsEmail docsKey stage letter =
  runReq defaultHttpConfig $ do
    let apiMode =
          if stage == "PROD"
            then "LIVE"
            else "TEST"
    let endpoint = https "www.docsaway.com" /: "app" /: "api" /: "rest" /: "mail.json"
    let apiConnection = object ["email" .= docsEmail, "key" .= docsKey]
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
            , "APIMode" .= (apiMode :: Text)
            , "APIReport" .= True
            , "PDFFile" .= letter
            , "PrintingStation" .= printingStation
            , "Recipient" .= recipient
            ]
    r <- req POST endpoint (ReqBodyJson payload) jsonResponse mempty
    return (responseBody r :: Value)

fromEnvOptional :: String -> String -> IO Text
fromEnvOptional var fallback = do
  envVar <- lookupEnv var
  return $ pack $ fromMaybe fallback envVar

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
