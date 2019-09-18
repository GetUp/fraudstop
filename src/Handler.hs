module Handler where

import AWSLambda.Events.APIGateway
  ( APIGatewayProxyRequest
  , APIGatewayProxyResponse(APIGatewayProxyResponse)
  , agprqPath
  , requestBody
  )
import Control.Exception (Exception, throw)
import Control.Lens ((<&>), (^.), set)
import qualified Control.Monad as CM
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
import Data.Aeson.TextValue (TextValue(TextValue))
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
          status <- mailer $ verificationEmail addresser token details requestId
          case status of
            Left err -> do
              print err
              pure $ responseMsg 500 "Unable to send verification email"
            Right _ -> pure responseOk
        Nothing -> pure $ responseMsg 400 "Incorrect format"
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
                             CM.when
                               (submitFoi details)
                               (do foiStatus <- mailer $ foiEmail addresser details
                                   print foiStatus)
                             CM.when
                               (emailMinister details)
                               (do ministerStatus <- mailer $ ministerEmail addresser details
                                   print ministerStatus)
                             CM.when
                               (emailMP details)
                               (do mps <- query conn selectMPs [postcode details]
                                   mpStatuses <- CM.forM mps $ \mp -> mailer $ mpEmail addresser details mp
                                   mapM_ print mpStatuses)
                             pure responseOk
                           _ -> throw BadLetter)
                else pure $ response 403
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

selectMPs :: Query
selectMPs = "select first_name, last_name, email from mps where ? = any(postcodes)"

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
      attachment = confirmationAttachment $ pack letter
   in (SG.mail [to] from subject content) {SG._mailAttachments = Just [attachment]}

confirmationEmailContent :: Text -> Text
confirmationEmailContent name =
  "Dear " <> name <>
  ", your request has been processed.  A review letter has been sent to Centrelink on your behalf. A digital copy of the letter has been attached for your reference.\n\nPS You may also receive BCC copies of several other emails, if you chose those options.  These are for your reference only; you do not need to do anything further with them."

confirmationAttachment :: Text -> MailAttachment
confirmationAttachment pdf =
  MailAttachment
    { SG._mailAttachmentContent = pdf
    , SG._mailAttachmentType = Just "application/pdf"
    , SG._mailAttachmentFilename = "review_request.pdf"
    , SG._mailAttachmentDisposition = Just Attachment
    , SG._mailAttachmentContentId = ""
    }

foiEmail :: (Text -> Text -> SG.Personalization) -> Details -> Mail () ()
foiEmail addresser details =
  let from = MailAddress (email details) (firstName details <> " " <> lastName details)
      to =
        (addresser "freedomofinformation@humanservices.gov.au" "Dept. of Human Services")
          {SG._personalizationBcc = Just [from]}
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

ministerEmail :: (Text -> Text -> SG.Personalization) -> Details -> Mail () ()
ministerEmail addresser details =
  let from = MailAddress (email details) (firstName details <> " " <> lastName details)
      to =
        (addresser "minister@humanservices.gov.au" "Minister for Government Services")
          {SG._personalizationBcc = Just [from]}
      subject = "Centrelink debt claim complaint"
      content = Just $ fromList [SG.mailContentText $ ministerEmailContent details]
   in SG.mail [to] from subject content

ministerEmailContent :: Details -> Text
ministerEmailContent d =
  let senderName = firstName d <> " " <> lastName d
   in "Dear Minister\n\nI am writing to you to complain. I have received a letter from Centrelink claiming that I have incorrectly reported my income.\n\nI believe that the letter of demand has been sent unfairly and has caused me to feel anxious and stressed.\n\nI request that you direct your staff or departmental officers to investigate how the debt claim against me was raised.\n\nFurthermore, I believe the current policy of automatically issued debt claims is grossly unfair and I request that you cease it immediately.\n\nI can be contacted on " <>
      email d <>
      " or " <>
      phone d <>
      ". Please either reply to this complaint or direct your staff or departmental officers to do so.\n\nBest regards\n\n" <>
      senderName

mpEmail :: (Text -> Text -> SG.Personalization) -> Details -> (Text, Text, Text) -> Mail () ()
mpEmail addresser details (mpFirstName, mpLastName, mpEmailAddress) =
  let mpName = mpFirstName <> " " <> mpLastName
      from = MailAddress (email details) (firstName details <> " " <> lastName details)
      to = (addresser mpEmailAddress mpName) {SG._personalizationBcc = Just [from]}
      subject = "Centrelink debt claim complaint"
      content = Just $ fromList [SG.mailContentText $ mpEmailContent mpName details]
   in SG.mail [to] from subject content

mpEmailContent :: Text -> Details -> Text
mpEmailContent recipientName d =
  let senderName = firstName d <> " " <> lastName d
      fullAddress = address d <> ", " <> suburb d <> " " <> postcode d
   in "Dear " <> recipientName <> ",\n\nI am a local constituent (address: " <> fullAddress <>
      ") writing to seek your urgent assistance with a Centrelink matter. \n\nLike many other people, I have recently received an automated letter from Centrelink asserting that I was overpaid in the past and now owe money. \n\nI am aware of the enormous number of mistakes being made in these letters, and I am seriously concerned my debt has been calculated incorrectly. \n\nI have submitted an application for review by a Centrelink Authorised Review Officer – but I am concerned about the reliability of the appeals process. Given the financial pressure placed on me by Centrelink’s potentially erroneous debt claim, I would appreciate your assistance in working with Centrelink staff and navigating the process to resolve this matter quickly. \n\nThank you very much for your help and support. I would appreciate it if you or your staff could contact me on " <>
      email d <>
      " or " <>
      phone d <>
      " at the first available opportunity to discuss my case. For reference, my Centrelink CRN is " <>
      crn d <>
      ".\n\nBest regards\n\n" <>
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

responseMsg :: Int -> Text -> APIGatewayProxyResponse Text
responseMsg n msg = APIGatewayProxyResponse n [] $ Just $ TextValue msg

dbUrl :: IO BSI.ByteString
dbUrl = do
  envUrl <- lookupEnv "DATABASE_URL"
  return $ BSI.packChars $ fromMaybe "postgresql://localhost/fraudstop" envUrl

tShow :: Show a => a -> Text
tShow = pack . show
