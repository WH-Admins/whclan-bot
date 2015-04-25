module Main where

import Network.SimpleIRC
import qualified Data.ByteString.Char8 as B

import WHBot

freenode = 
  (mkDefaultConfig "irc.freenode.net" "WreakingBot") 
    {cChannels = ["#wreaking-test"], cEvents = events}

main = do
  --connect freenode True True
  connect freenode False True
