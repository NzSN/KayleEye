{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode)
import Data.Text (Text)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  -- Create the request
  let requestObject = object [ "merge_request_iid" .= ("lll" :: Text) ]

  initialRequest <- parseRequest "http://gpon.git.com:8011/api/v4/projects/51/merge_requests/4/merge?private_token=D_-yvMKXNJcqpQxZr_CU"
  print $ encode requestObject
  let request = initialRequest { method = "PUT", requestBody = RequestBodyLBS $ encode requestObject }
  print request

  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
