module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Lib
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import qualified System.Environment as System

myToken :: IO BC.ByteString
myToken = do
  env <- System.getEnvironment
  let tokenEnv = filter ((== "NOAA_TOKEN") . fst) env
  let token =
        if null tokenEnv
          then ""
          else snd (head tokenEnv)
  return (BC.pack token)

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method $
    setRequestHost host $
      setRequestHeader "token" [token] $
        setRequestPath path $
          setRequestSecure True $
            setRequestPort
              443
              defaultRequest

getDatasetsRequest :: IO Request
getDatasetsRequest = do
  token <- myToken
  return (buildRequest token noaaHost "GET" apiPath)

main :: IO ()
main = do
  request <- getDatasetsRequest
  response <- httpLBS request
  let status = getResponseStatus response
  if statusCode status == 200
    then do
      print "saving response to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
      print "response saved"
    else print ("request failed with error: " ++ show status)
