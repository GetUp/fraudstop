module Main where

import AWSLambda.Events.APIGateway (apiGatewayMain)
import Control.Exception (catch)
import Handler (CustomException, handler)

main :: IO ()
main = apiGatewayMain handler `catch` errorHandler
  where
    errorHandler :: CustomException -> IO ()
    errorHandler = print
