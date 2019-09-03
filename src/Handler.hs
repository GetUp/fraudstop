module Handler where

import AWSLambda.Events.APIGateway
  ( APIGatewayProxyRequest
  , APIGatewayProxyResponse(APIGatewayProxyResponse)
  , requestBody
  )
import Control.Lens ((^.))

import Data.Aeson (FromJSON, ToJSON, decode, defaultOptions, genericToEncoding, toEncoding)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
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

detailDecode :: Text -> Maybe Details
detailDecode = decode . fromStrict . encodeUtf8

handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
  let body = request ^. requestBody
  case detailDecode =<< body of
    Just details -> do
      print details
      pure $ APIGatewayProxyResponse 404 [] Nothing
    _ -> pure $ APIGatewayProxyResponse 404 [] Nothing
