module Mocks where

import qualified AWSLambda.Events.APIGateway as APIG
import Data.Aeson (toJSON)
import Data.Aeson.TextValue (TextValue(TextValue))
import Data.ByteString.Internal (ByteString, packChars)
import Data.HashMap.Strict (fromList)
import Data.Text (Text)

request :: String -> [(ByteString, Maybe ByteString)] -> Text -> APIG.APIGatewayProxyRequest Text
request path params body =
  APIG.APIGatewayProxyRequest
    { APIG._agprqResource = "/{proxy+}"
    , APIG._agprqPath = packChars path
    , APIG._agprqHttpMethod = "POST"
    , APIG._agprqHeaders =
        [ ("X-Forwarded-Proto", "https")
        , ("CloudFront-Is-Desktop-Viewer", "true")
        , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
        , ("X-Amz-Cf-Id", "nBsWBOrSHMgnaROZJK1wGCZ9PcRcSpq_oSXZNQwQ10OTZL4cimZo3g==")
        , ("Accept-Encoding", "gzip, deflate, lzma, sdch, br")
        , ("CloudFront-Forwarded-Proto", "https")
        , ("Accept-Language", "en-US,en;q=0.8")
        , ("CloudFront-Is-Tablet-Viewer", "false")
        , ("Upgrade-Insecure-Requests", "1")
        , ("CloudFront-Viewer-Country", "US")
        , ( "User-Agent"
          , "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.82 Safari/537.36 OPR/39.0.2256.48")
        , ("CloudFront-Is-Mobile-Viewer", "false")
        , ("Host", "apig.com")
        , ("X-Forwarded-Port", "443")
        , ("CloudFront-Is-SmartTV-Viewer", "false")
        , ("Via", "1.1 fb7cca60f0ecd82ce07790c9c5eef16c.cloudfront.net (CloudFront)")
        , ("X-Forwarded-For", "192.168.100.1, 192.168.1.1")
        ]
    , APIG._agprqQueryStringParameters = params
    , APIG._agprqPathParameters = fromList [("proxy", "hello")]
    , APIG._agprqStageVariables = fromList [("stageVarName", "stageVarValue")]
    , APIG._agprqRequestContext =
        APIG.ProxyRequestContext
          { APIG._prcPath = Nothing
          , APIG._prcAccountId = "123456789012"
          , APIG._prcResourceId = "us4z18"
          , APIG._prcStage = "test"
          , APIG._prcRequestId = "41b45ea3-70b5-11e6-b7bd-69b5aaebc7d9"
          , APIG._prcIdentity =
              APIG.RequestIdentity
                { APIG._riCognitoIdentityPoolId = Just ""
                , APIG._riAccountId = Just ""
                , APIG._riCognitoIdentityId = Just ""
                , APIG._riCaller = Just ""
                , APIG._riApiKey = Just ""
                , APIG._riSourceIp = Nothing
                , APIG._riCognitoAuthenticationType = Just ""
                , APIG._riCognitoAuthenticationProvider = Just ""
                , APIG._riUserArn = Just ""
                , APIG._riUserAgent =
                    Just
                      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.82 Safari/537.36 OPR/39.0.2256.48"
                , APIG._riUser = Just ""
                }
          , APIG._prcResourcePath = "/{proxy+}"
          , APIG._prcHttpMethod = "POST"
          , APIG._prcApiId = "wt6mne2s9k"
          , APIG._prcProtocol = "HTTP/1.1"
          , APIG._prcAuthorizer =
              Just
                APIG.Authorizer
                  { APIG._aPrincipalId = Just "test-principalId"
                  , APIG._aClaims =
                      fromList [("email", toJSON ("test@example.com" :: Text)), ("email_verified", toJSON True)]
                  , APIG._aContext = fromList [("custom_context", toJSON (10 :: Int))]
                  }
          }
    , APIG._agprqBody = Just $ TextValue body
    }
