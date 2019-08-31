import AWSLambda.Events.APIGateway (APIGatewayProxyResponse(APIGatewayProxyResponse))
import Data.Aeson.TextValue (TextValue(TextValue))
import Data.Text (Text, unpack)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, execute_)
import Test.Hspec

import Handler
import Mocks

main :: IO ()
main = do
  url <- dbUrl
  conn <- connectPostgreSQL url
  hspec $
    before_ (setupDb conn) $
    describe "/connect" $ do
      let queryParams = [("campaign_id", Just "1")]
      let postParams = [("CallUUID", "xxxxx"), ("From", "61411111111")]
      it "should give an intro, proceed to the first call when 1 is pressed, redirect if no input" $ do
        reqResponse <- handler $ Mocks.request "/connect" queryParams postParams
        reqResponse `shouldMatchBody` "<Speak language=\"en-GB\" voice=\"MAN\">Welcome to the Test campaign.</Speak>"

setupDb :: Connection -> IO ()
setupDb conn = do
  flushDb conn
  return ()

flushDb :: Connection -> IO ()
flushDb conn = do
  _ <- execute_ conn "truncate targets, calls, callers, campaigns restart identity"
  return ()

shouldMatchBody :: APIGatewayProxyResponse Text -> Text -> Expectation
shouldMatchBody (APIGatewayProxyResponse _ _ (Just (TextValue body))) fragment =
  unpack body `shouldContain` unpack fragment
shouldMatchBody (APIGatewayProxyResponse 404 _ _) _ = error "response was 404"
shouldMatchBody _ _ = error "Request was probably malformed"

shouldNotMatchBody :: APIGatewayProxyResponse Text -> Text -> Expectation
shouldNotMatchBody (APIGatewayProxyResponse _ _ (Just (TextValue body))) fragment =
  unpack body `shouldNotContain` unpack fragment
shouldNotMatchBody (APIGatewayProxyResponse 404 _ _) _ = error "response was 404"
shouldNotMatchBody _ _ = error "Request was probably malformed"
