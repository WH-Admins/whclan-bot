{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

chanFromMsg :: MIrc -> IrcMessage -> IO (Maybe B.ByteString)
chanFromMsg s msg = 
  case mChan msg of
    Just channel -> 
      case B.unpack channel of
        ('#':_) -> return $ Just channel
        _ -> failReturn 
    Nothing -> failReturn
  where
    failReturn = 
      do
        sendMsg s (fromJust $ mOrigin msg) "talk to me on a channel you fool" 
        return Nothing

botsnack :: EventFunc
botsnack s msg = 
  chanFromMsg s msg >>= \maybeChan ->
    case maybeChan of
      Just channel ->
        if (mMsg msg) == "|botsnack"
          then 
            sendMsg s channel "delicious, mkay"
          else return ()
      Nothing -> return ()

echoEvent :: EventFunc
echoEvent s message =
  case sequence (map ($ message) [mOrigin, Just . mMsg]) of
    Just [origin, msg] -> sendMsg s origin msg
    Nothing -> return ()

data BState = BState { bsValue :: Int }

events = 
  map Privmsg
    [ botsnack ]

freenode = 
  (mkDefaultConfig "irc.freenode.net" "WreakingBot") 
    {cChannels = ["#wreaking-test"], cEvents = events}

main = do
  --connect freenode True True
  connect freenode False True
