{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric  #-}

module API where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text hiding (dropWhile)
import Data.Aeson
import Control.Applicative
import Control.Monad
import GHC.Generics
import BV
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import Network.HTTP

--hey remove this before making code public!!!!
key = "0373FX9sGnppdRBUxTroWBx8Rb4EJ53kFAYeMNgpvpsH1H"

callServer path = "http://icfpc2013.cloudapp.net/" ++ (dropWhile ('/'==) path) ++ "?auth=" ++ key

statsURL = callServer "status"
trainURL = callServer "train"

decode_string s = decode $ BS.pack s

getJSON :: String -> IO BS.ByteString
getJSON url = do
   json <- simpleHTTP (getRequest url) >>= getResponseBody
   return (BS.pack json)

call :: FromJSON b => String -> IO b
call url =
 -- Get JSON data and decode it
 (>>= maybe (ioError $ userError "deserialization error") return) (decode <$> (getJSON url))

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

-- interface TrainingRequest {
--    size?: number;
--    operators?: string[];
-- }

data TrainingRequest = TrainingRequest {
    reqSize      :: Maybe Int,
    reqOperators :: Maybe Array --TODO: take operators
} deriving (Show)

instance ToJSON TrainingRequest where
    toJSON TrainingRequest{..} = object $ catMaybes
                                           [ ("size" .=) <$> reqSize
                                           , ("operators" .=) <$> reqOperators ]

--TODO: add body to request
train = call trainURL :: IO Problem



--testing methods
testStatic :: String -> IO BS.ByteString
testStatic s = do
  return (BS.pack s)

--testProblem = "{\"id\":\"ZH35UqFFcfzebGjoMB8rgYpA\",\"size\":17,\"operators\":[\"and\",\"if0\",\"or\",\"shr1\",\"shr16\",\"shr4\",\"xor\"]}"

