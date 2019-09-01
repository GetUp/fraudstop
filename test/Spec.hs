import AWSLambda.Events.APIGateway (APIGatewayProxyResponse(APIGatewayProxyResponse))
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(Object)
  , Value
  , (.=)
  , decode
  , defaultOptions
  , encode
  , genericToEncoding
  , object
  , toEncoding
  , withObject
  )
import Data.Text (Text, unpack)
import Database.PostgreSQL.Simple (Connection, Only(Only), connectPostgreSQL, execute_, query_)
import Test.Hspec

import Handler
import Mocks

main :: IO ()
main = do
  url <- dbUrl
  conn <- connectPostgreSQL url
  hspec $
    before_ (setupDb conn) $
    describe "/begin" $ do
      let queryParams = [] -- [("campaign_id", Just "1")]
      it "persists the details and sends a confirmation email" $ do
        reqResponse <- handler $ Mocks.request "/begin" queryParams details
        reqResponse `shouldBe` APIGatewayProxyResponse 200 [] Nothing
        [Only requestId] <- query_ conn "select id from user_requests limit 1" :: IO [Only Int]
        requestId `shouldBe` 1
        -- [Only (Object details)] <- query_ conn "select details from user_requests limit 1" :: IO [Only Value]
        -- print details
        -- details `shouldBe` "123456789x"
        -- check db
        -- expect sendgrid api request

setupDb :: Connection -> IO ()
setupDb conn = do
  flushDb conn
  return ()

flushDb :: Connection -> IO ()
flushDb conn = do
  _ <- execute_ conn "truncate user_requests, api_logs restart identity cascade"
  return ()

details :: Text
details =
  "{\"firstName\":\"alice\",\"lastName\":\"citizen\",\"email\":\"tim+alicecitizen@mcewan.it\",\"address\":\"7 henry dr\",\"suburb\":\"picnic point\",\"postcode\":\"2341\",\"dob\":\"01/01/1900\",\"phone\":\"0123456789\",\"crn\":\"123456789x\",\"debtReason\":\"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\",\"emailMP\":true,\"emailMinister\":true,\"submitFoi\":true,\"personalCircumstances\":[\"Addiction\",\"sfdg sdfgsdfg dsfg\"]}"
