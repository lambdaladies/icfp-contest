{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric  #-}

module Data where

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
import Network.HTTP.Types (methodPost)
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

--run first:
--    cabal install http-conduit
--    cabal install aeson

--hey remove this before making code public!!!!
key = "0373FX9sGnppdRBUxTroWBx8Rb4EJ53kFAYeMNgpvpsH1H"

callServer path = "http://icfpc2013.cloudapp.net/" ++ (dropWhile ('/'==) path) ++ "?auth=" ++ key

postData :: TrainingRequest -> IO TrainingProblem
postData jsonBody = do
  initRequest <- parseUrl trainURL
  let request = initRequest { method = methodPost
                            , requestHeaders = [("Content-Type", "application/json")]
                            , requestBody = RequestBodyLBS (encode jsonBody)
                            }
  res <- withManager $ httpLbs request
  let eitherJson = eitherDecode (responseBody res)
  case eitherJson of
    Left e -> error ("Could not decode: " ++ show (responseBody res)
              ++ "\n" ++ show e)
    Right training -> return training

testPost :: TrainingRequest -> IO ()
testPost jsonBody = postData jsonBody >>= print

statsURL = callServer "status"
trainURL = callServer "train"

getJSON :: String -> IO BS.ByteString
getJSON url = do
   json <- simpleHTTP (getRequest url) >>= getResponseBody
   return (BS.pack json)

call :: FromJSON b => String -> IO b
call url =
 -- Get JSON data and decode it
 (>>= maybe (ioError $ userError "deserialization error") return) (decode <$> (getJSON url))

   --interface TrainingProblem {
   --  challenge: string;
   --  id: string;
   --  size: number;
   --  operators: string[];
   --}

data TrainingProblem =
   TrainingProblem { id        :: String,
                     size      :: Int,
                     operators :: [Ops],
                     challenge :: String -- TODO: parse into expression
                    } deriving (Show, Generic)

instance FromJSON TrainingProblem
instance ToJSON TrainingProblem

instance FromJSON Ops where
  parseJSON (String s) = return $ read $ unpack s
  parseJSON op = error ("failed to parse operator: " ++ show op)

instance ToJSON Ops where
  toJSON ops = String (pack $ show ops)

   --interface TrainingRequest {
   --    size?: number;
   --    operators?: string[];
   --   }

data TrainingRequest =
   TrainingRequest {
                     reqSize       :: Maybe Int,
                     reqOperators  :: [Ops]
                   } deriving (Show)

instance ToJSON TrainingRequest where
  toJSON tReq = case reqSize tReq of
    Nothing -> object ["operators" .= (toJSON (reqOperators tReq))]
    Just size -> object [ "size" .= size
                        , "operators" .= (toJSON (reqOperators tReq)) ]

--TODO: add body to request
train = call trainURL :: IO TrainingProblem

 --interface Problem {
 --   id: string;
 --   size: number;
 --   operators: string[];
 --   solved?: boolean;
 --   timeLeft?: number
 -- }