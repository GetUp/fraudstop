module Main where

import qualified Data.Aeson as Aeson
import Lib

import AWSLambda

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO [Int]
handler evt = do
  putStrLn "This should go to logs"
  someFunc
  print evt
  pure [1, 2, 3]
