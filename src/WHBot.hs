module WHBot 
  ( events ) where

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad (void)

import WHBot.Utilities

botsnack :: B.ByteString -> EventFunc
botsnack channel s msg = 
  if (mMsg msg) == "|botsnack"
    then sendMsg s channel "delicious, mkay"
    else return ()

data BState = BState { bsValue :: Int }

events = 
  map (Privmsg . doWithChan)
    [ botsnack ]
