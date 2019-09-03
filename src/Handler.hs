module Handler where

import AWSLambda.Events.APIGateway
  ( APIGatewayProxyRequest
  , APIGatewayProxyResponse(APIGatewayProxyResponse)
  , requestBody
  )
import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON, decode, defaultOptions, genericToEncoding, toEncoding)

import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

-- import Data.Typeable (typeOf)
import GHC.Generics (Generic)

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

instance ToJSON Details where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Details

recode :: Text -> ByteString
recode = fromStrict . encodeUtf8

handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
  let body = request ^. requestBody
  case body of
    Just bod -> do
      let deets = decode (recode bod) :: Maybe Details
      print $ deets
      -- case (decode bod :: Maybe Details) of
      --   Just deets -> pure $ response 200
      --   Nothing -> pure $ response 403
      pure $ APIGatewayProxyResponse 404 [] Nothing
    _ -> pure $ APIGatewayProxyResponse 404 [] Nothing
