module Main where

import Network.SimpleIRC
import qualified Data.ByteString.Char8 as B
import Control.Monad (void)

import WHBot

myConfig :: [IrcEvent] -> IrcConfig
myConfig myEvents = 
  (mkDefaultConfig "irc.freenode.net" "WreakingBot") 
    {cChannels = ["#wreaking"], cEvents = myEvents}

main :: IO ()
main = 
  void $
    do
      myEvents <- events
      connect (myConfig myEvents) False True
