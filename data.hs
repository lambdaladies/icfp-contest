{-# LANGUAGE OverloadedStrings, DeriveGeneric  #-}
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text hiding (dropWhile)
import Data.Aeson
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit
import GHC.Generics
import Syntax

--run first:
--	cabal install http-conduit 
-- 	cabal install aeson

--hey remove this before making code public!!!!
key = "0373FX9sGnppdRBUxTroWBx8Rb4EJ53kFAYeMNgpvpsH1H"

callServer path = "http://icfpc2013.cloudapp.net/" ++ (dropWhile ('/'==) path) ++ "?auth=" ++ key


statsURL = callServer "status"
trainURL = callServer "train"

getJSON :: String -> IO BS.ByteString
getJSON url = simpleHttp url

   --interface TrainingProblem {
   --  challenge: string;
   --  id: string;
   --  size: number;
   --  operators: string[];
   --}

data TrainingProblem = 
   TrainingProblem { id    		:: String,
   					 size        :: Int,
   					 operators	:: Array, --TODO: parse into operators
   					 challenge   :: String
                   } deriving (Show, Generic)

instance FromJSON TrainingProblem
instance ToJSON TrainingProblem

--TODO: add body to request
--testPrint = encode (TrainingProblem {Main.id = "test", Main.size=4, Main.operators = (Array ["c"]), challenge = "testtest"} )

train = 
 -- Get JSON data and decode it
 (eitherDecode <$> (getJSON trainURL)) :: IO (Either String TrainingProblem)

 
 --interface Problem {
 --   id: string;
 --   size: number;
 --   operators: string[];
 --   solved?: boolean;
 --   timeLeft?: number
 -- }



