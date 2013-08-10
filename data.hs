{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric  #-}
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

--run first:
--	cabal install http-conduit 
-- 	cabal install aeson

--hey remove this before making code public!!!!
key = "0373FX9sGnppdRBUxTroWBx8Rb4EJ53kFAYeMNgpvpsH1H"

callServer path = "http://icfpc2013.cloudapp.net/" ++ (dropWhile ('/'==) path) ++ "?auth=" ++ key

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
                     operators :: Array, --TODO: parse into operators
                     challenge :: String
                   } deriving (Show, Generic)

instance FromJSON TrainingProblem
instance ToJSON TrainingProblem

   --interface TrainingRequest {
   --    size?: number;
   --    operators?: string[];
   --   }

data TrainingRequest =
   TrainingRequest {
                     reqSize :: Maybe Int,
                     reqOperators  :: Maybe Array --TODO: take operators
                   } deriving (Show)

instance ToJSON TrainingRequest where
  toJSON TrainingRequest{..} = object $ catMaybes
                                           [ ("size" .=) <$> reqSize
                                           , ("operators" .=) <$> reqOperators ]

--TODO: add body to request
train = call trainURL :: IO TrainingProblem


 --interface Problem {
 --   id: string;
 --   size: number;
 --   operators: string[];
 --   solved?: boolean;
 --   timeLeft?: number
 -- }



