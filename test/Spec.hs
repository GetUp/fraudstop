import AWSLambda.Events.APIGateway (APIGatewayProxyResponse(APIGatewayProxyResponse))
import qualified Data.ByteString.Internal as BSI
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, Only(Only), Query, connectPostgreSQL, execute, execute_, query_)
import System.Environment (lookupEnv)
import Test.Hspec (before_, context, describe, hspec, it, shouldBe, xit)

import Handler (handler, secureToken)
import qualified Mocks as Mock

main :: IO ()
main = do
  url <- testDbUrl
  conn <- connectPostgreSQL url
  hspec $
    before_ (setupDb conn) $ do
      describe "/begin" $
        xit "persists the details and sends a verification email" $ do
          reqResponse <- handler $ Mock.request "/begin" [] details
          reqResponse `shouldBe` APIGatewayProxyResponse 200 [] Nothing
          [Only requestsCount] <-
            query_ conn "select count(id) from user_requests where details is not null" :: IO [Only Int]
          requestsCount `shouldBe` 1
        -- [Only (Object details)] <- query_ conn "select details from user_requests limit 1" :: IO [Only Value]
        -- print details
        -- details `shouldBe` "123456789x"
        -- expect sendgrid api request?
      describe "/confirm" $
        before_ (setupConfirm conn) $ do
          context "with an invalid token" $ do
            let body = invalidVerification
            it "refuses to do anything" $ do
              reqResponse <- handler $ Mock.request "/confirm" [] body
              reqResponse `shouldBe` APIGatewayProxyResponse 403 [] Nothing
          context "with a valid token" $ do
            let body = validVerification
            it "processes the request" $ do
              reqResponse <- handler $ Mock.request "/confirm" [] body
              reqResponse `shouldBe` APIGatewayProxyResponse 200 [] Nothing
              [Only requestId] <-
                query_ conn "select id from user_requests where processed_at is not null " :: IO [Only Int]
              requestId `shouldBe` 1
      describe "#secureToken" $ do
        let salt = "abcdefg"
        let requestId = 1
        it "hashes the email address" $
          secureToken salt requestId `shouldBe` "ec5acefa44cf8383f4ebaff94b6bfa200a9fdbe92aaf11481907c040c4a11b54"

setupConfirm :: Connection -> IO ()
setupConfirm conn = do
  _ <- execute conn insertTestDetails [details]
  return ()

setupDb :: Connection -> IO ()
setupDb conn = do
  flushDb conn
  return ()

flushDb :: Connection -> IO ()
flushDb conn = do
  _ <- execute_ conn "truncate user_requests, api_logs restart identity cascade"
  return ()

testDbUrl :: IO BSI.ByteString
testDbUrl = do
  envUrl <- lookupEnv "DATABASE_URL"
  return $ BSI.packChars $ fromMaybe "postgresql://localhost/fraudstop" envUrl

insertTestDetails :: Query
insertTestDetails = "insert into user_requests(created_at, details) values(now(), ?)"

details :: Text
details =
  "{\"firstName\":\"alice\",\"lastName\":\"citizen\",\"email\":\"tim+alicecitizen@mcewan.it\",\"address\":\"7 henry dr\",\"suburb\":\"picnic point\",\"postcode\":\"2341\",\"dob\":\"01/01/1900\",\"phone\":\"0123456789\",\"crn\":\"123456789x\",\"debtReason\":\"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\",\"emailMP\":true,\"emailMinister\":true,\"submitFoi\":true,\"personalCircumstances\":[\"Addiction\",\"sfdg sdfgsdfg dsfg\"]}"

validVerification :: Text
validVerification = "{\"requestId\":1,\"token\":\"ec5acefa44cf8383f4ebaff94b6bfa200a9fdbe92aaf11481907c040c4a11b54\"}"

invalidVerification :: Text
invalidVerification = "{\"requestId\":1,\"token\":\"xxx\"}"
