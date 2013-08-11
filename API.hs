{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric  #-}

module API where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Char (toUpper)
import Data.Aeson
import Numeric (showHex)
import Control.Applicative
import Control.Monad
import GHC.Generics
import BV
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import Network.HTTP
import Network.HTTP.Types (methodPost)
import Network.HTTP.Conduit

--hey remove this before making code public!!!!
key = "0373FX9sGnppdRBUxTroWBx8Rb4EJ53kFAYeMNgpvpsH1H"

callServer path = "http://icfpc2013.cloudapp.net/" ++ (dropWhile ('/'==) path) ++ "?auth=" ++ key

statsURL = callServer "status"
trainURL = callServer "train"
problemsURL = callServer "myproblems"

decode_string s = decode $ BS.pack s

--todo: add bodies
--postRequestWithBody url "json" "test"
getJSON :: String -> IO BS.ByteString
getJSON url = do
   json <- simpleHTTP (getRequest url) >>= getResponseBody
   return (BS.pack json)

-- makes a post request for a given json-request and yields (hopefully)
postData :: (ToJSON a, FromJSON b) => a -> String -> IO b
postData jsonBody url = do
  initRequest <- parseUrl trainURL
  let request =
        initRequest { method = methodPost
                    , requestHeaders = [("Content-Type", "application/json")]
                    , requestBody = RequestBodyLBS (encode jsonBody)
                    }
  res <- withManager $ httpLbs request
  let eitherJson = eitherDecode (responseBody res)
  case eitherJson of
    Left e -> error ("Could not decode: " ++ show (responseBody res)
              ++ "\n" ++ show e)
    Right training -> return training

-- prints the response of a given training request
testPost :: ToJSON a => String -> a -> IO ()
testPost url jsonBody = (postData jsonBody url :: IO Problem) >>= print

 -- Get JSON data and decode it
call :: FromJSON b => String -> IO b
call url = (>>= maybe (ioError $ userError "deserialization error")
                      return)
           (decode <$> (getJSON url))

-- this represents both TrainingProblem and Problem
data Problem = Problem {
    problemId :: String,
    problemSize :: Int,
    problemOperators :: BV.Operators,
    problemChallenge :: Maybe String, -- training problems only
    problemSolved :: Bool,
    problemTimeLeft :: Maybe Int
} deriving (Show)

instance FromJSON Problem where
    parseJSON (Object o) = Problem <$>
                           o .: "id" <*>
                           o .: "size" <*>
                           liftM parse_opstrings (o .: "operators") <*>
                           o .:? "challenge" <*>
                           liftM (fromMaybe False) (o .:? "solved") <*>
                           o .:? "timeLeft"
    parseJSON _          = mzero

data TrainingRequest =
   TrainingRequest {
                     reqSize       :: Maybe Int,
                     reqOperators  :: [Ops]
                   } deriving (Show)

instance ToJSON TrainingRequest where
  toJSON tReq = case reqSize tReq of
    Nothing    -> object ["operators" .= toJSON (reqOperators tReq)]
    Just rSize -> object [ "size" .= rSize ]

instance ToJSON Ops where
  toJSON ops = String (T.pack $ show ops)

--TODO: add body to request
train = call trainURL :: IO Problem
myProblems = call problemsURL :: IO [Problem]


-- Eval requests and responses

data EvalRequest = EvalRequest {
    evalReqId        :: String,
    --evalReqProgram :: Maybe Program, -- we don't use this
    evalReqArguments :: [Vector]
} deriving (Show)

instance ToJSON EvalRequest where
    toJSON evalReq = object ["id"        .= toJSON (evalReqId evalReq),
                             "arguments" .= toJSON (map to_hex (evalReqArguments evalReq))]

to_hex :: Vector -> String
to_hex v = "0x" ++ map toUpper (showHex v "")

data EvalResponse = EvalResponse {
    evalRespStatus  :: String,
    evalRespOutputs :: Maybe [Vector],
    evalRespMessage :: Maybe String
}

instance FromJSON EvalResponse where
    parseJSON (Object o) = EvalResponse <$>
                           o .: "status" <*>
                           o .:? "outputs" <*>
                           o .:? "message"
    parseJSON _          = mzero


-- Guesses and guess responses

data GuessResponse = GuessResponse {
  status :: String,
  values :: Maybe [Vector],
  message :: Maybe String,
  lightning :: Maybe Bool
} deriving (Show)

instance FromJSON GuessResponse where
  parseJSON (Object o) = GuessResponse <$>
                         o .: "status" <*>
                         o .:? "values" <*>
                         o .:? "message" <*>
                         o .:? "lightning"
  parseJSON _ = mzero

--testing methods
testStatic :: String -> IO BS.ByteString
testStatic s = do
  return (BS.pack s)

--testProblem = "{\"id\":\"ZH35UqFFcfzebGjoMB8rgYpA\",\"size\":17,\"operators\":[\"and\",\"if0\",\"or\",\"shr1\",\"shr16\",\"shr4\",\"xor\"]}"
