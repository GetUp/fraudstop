module Main where

import AWSLambda.Events.APIGateway (apiGatewayMain)
import Handler (handler)

main :: IO ()
main = apiGatewayMain handler `catch` errorHandler
  where
    errorHandler :: CustomException -> IO ()
    errorHandler err = putStrLn (show err)
