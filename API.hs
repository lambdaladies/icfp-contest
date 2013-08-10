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

-- interface TrainingProblem {
--    challenge: string;
--    id: string;
--    size: number;
--    operators: string[];
-- }

-- interface Problem {
--    id: string;
--    size: number;
--    operators: string[];
--    solved?: boolean;
--    timeLeft?: number
-- }

-- this represents both TrainingProblem and Problem
data Problem = Problem {
    problemId :: String,
    problemSize :: Int,
    problemOperatorStrings :: [String],
    problemChallenge :: Maybe String, -- training problems only
    problemSolvedMaybe :: Maybe Bool,
    problemTimeLeft :: Maybe Int
} deriving (Show)

problemOperators p = parse_opstrings $ problemOperatorStrings p

problemSolved p = bool_or_false $ problemSolvedMaybe p
bool_or_false (Just b) = b
bool_or_false Nothing  = False

instance FromJSON Problem where
    parseJSON (Object o) = Problem <$>
                           o .: "id" <*>
                           o .: "size" <*>
                           o .: "operators" <*>
                           o .: "challenge" <*>
                           o .: "solved" <*>
                           o .: "timeLeft"
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

