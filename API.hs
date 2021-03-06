{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module API where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Char (toUpper)
import Data.Aeson
import Numeric (showHex, readHex)
import Control.Applicative
import Control.Monad
import GHC.Generics
import BV
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Network.HTTP
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Status (statusCode, statusMessage)
import Network.HTTP.Conduit

key = "THIS_WAS_OUR_TOKEN_vpsH1H"

callServer path = "http://icfpc2013.cloudapp.net/" ++ (dropWhile ('/'==) path) ++ "?auth=" ++ key

statsURL    = callServer "status"
trainURL    = callServer "train"
problemsURL = callServer "myproblems"
evalURL     = callServer "eval"
guessURL    = callServer "guess"

decode_string s = decode $ BS.pack s

getJSON :: String -> IO BS.ByteString
getJSON url = do
    json <- simpleHTTP (getRequest url) >>= getResponseBody
    return (BS.pack json)

-- makes a post request for a given json-request and yields (hopefully)
postData :: (ToJSON a, FromJSON b, Show b) => a -> (String -> b) -> Int -> String -> IO b
postData jsonBody failRequest responseTimeout url = do
  putStrLn $ "POST " ++ url
  print $ encode jsonBody

  initRequest <- parseUrl url
  let request =
        initRequest { method = methodPost
                    , requestHeaders = [("Content-Type", "application/json")]
                    , requestBody = RequestBodyLBS (encode jsonBody)
                    }
  response <- try (withManagerSettings
                    (def { managerResponseTimeout = Just responseTimeout })
                    (httpLbs request))
  result <-
    case response of
      Left ResponseTimeout ->
      -- retry with higher responseTimeout value
        postData jsonBody failRequest (increase_timeout responseTimeout) url
      Left (StatusCodeException s _ _) ->
        case statusCode s of
          410  -> return $ failRequest "Timeout"
          -- got "try again later", just retry request
          429  -> putStr ("\nretry request\n") >>
                    postData jsonBody failRequest responseTimeout url
          _    -> return $ failRequest (B.unpack $ statusMessage s)
      Left e ->  return (failRequest $ show (e :: HttpException))
      Right res -> getPostResult res failRequest
  print result
  return result
 where
  getPostResult res failRequest = do
    let status = responseStatus res
    case statusCode status of
       200 ->  do
               let eitherJson = eitherDecode (responseBody res)
               case eitherJson of
                 Left e -> return $ failRequest ("Could not decode:\n"
                                        ++ show (responseBody res)
                                        ++ "\n" ++ show e)
                 Right bVal -> return bVal
       410  -> return $ failRequest "Timeout"
       -- got "try again later", just retry request
       429  -> putStr ("\nretry request\n") >>
                 postData jsonBody failRequest responseTimeout url
       _    -> return $ failRequest (B.unpack $ statusMessage status)

-- prints the response of a given training request

testTrain :: TrainingRequest -> IO Problem
testTrain jsonBody =
  postData jsonBody dummy_problem default_res_timeout trainURL :: IO Problem

dummy_problem msg = Problem { problemId = "Error: " ++ msg,
                              problemSize = 0,
                              problemOperators = Operators { unary=[], binary=[], ternary=[], folds=[], specials=[], vars=[] },
                              problemChallenge = Nothing,
                              problemSolved = False,
                              problemTimeLeft = Nothing }

default_res_timeout = 10000000
increase_timeout s  = s + 5000000

testPost :: ToJSON a => String -> a -> IO ()
testPost url jsonBody =
  (postData jsonBody dummy_problem default_res_timeout url :: IO Problem)
    >>= print

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
    Nothing -> object [ops]
    Just rSize -> object [ "size" .= rSize, ops]
    where
     ops = "operators" .= (toJSON (reqOperators tReq))

instance ToJSON Ops where
    toJSON ops = String (T.pack $ show ops)

instance ToJSON Expr where
  toJSON expr = String (T.pack $ show_program expr)

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

-- fixme: error handling
parse_vectors (Just xs) = Just (map (fromJust . from_hex) xs)
parse_vectors Nothing   = Nothing

from_hex :: String -> Maybe Vector
from_hex ('0':'x':s) = case readHex s of
                         [(v, "")] -> Just v
                         _         -> Nothing
from_hex _           = Nothing

data EvalResponse = EvalResponse {
    evalRespStatus  :: String,
    evalRespOutputs :: Maybe [Vector],
    evalRespMessage :: Maybe String
} deriving (Show)

instance FromJSON EvalResponse where
    parseJSON (Object o) = EvalResponse <$>
                           o .: "status" <*>
                           liftM parse_vectors (o .:? "outputs") <*>
                           o .:? "message"
    parseJSON _          = mzero


-- Guesses and guess responses

-- interface Guess {
--     id: string;
--     program: string;
-- }

data Guess = Guess {
    guessId      :: String,
    guessProgram :: Expr
} deriving (Show)

instance ToJSON Guess where
  toJSON gReq = object [ "id"      .= guessId gReq
                       , "program" .= show_program (guessProgram gReq)
                       ]

-- interface GuessResponse {
--     status: string;
--     values?: string[];
--     message?: string;
--     lightning?: bool;
-- }

data GuessResponse = GuessResponse {
    guessRespStatus    :: String,
    guessRespValues    :: Maybe [Vector],
    guessRespMessage   :: Maybe String,
    guessRespLightning :: Maybe Bool
} deriving (Show)

instance FromJSON GuessResponse where
    parseJSON (Object o) = GuessResponse <$>
                           o .: "status" <*>
                           liftM parse_vectors (o .:? "values") <*>
                           o .:? "message" <*>
                           o .:? "lightning"
    parseJSON _ = mzero

--testing methods
testStatic :: String -> IO BS.ByteString
testStatic s = do
    return (BS.pack s)

--testProblem = "{\"id\":\"ZH35UqFFcfzebGjoMB8rgYpA\",\"size\":17,\"operators\":[\"and\",\"if0\",\"or\",\"shr1\",\"shr16\",\"shr4\",\"xor\"]}"
