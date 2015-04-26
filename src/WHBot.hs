module WHBot 
  ( events ) where

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad (void)

import WHBot.Behaviors

botsnackB :: ChannelBehavior
botsnackB = ChannelBehavior $ \channel (s, msg) -> 
  if mMsg msg == "!botsnack"
    then sendMsg s channel "delicious, mkay"
    else return ()

linkB :: ChannelBehavior
linkB = ChannelBehavior $ \channel (s, msg) ->
  let contentWords = words . B.unpack $ mMsg msg
  in
    if head contentWords == "!link" then
      sendMsg s (fromMaybe "" $ mOrigin msg) $
        case tail contentWords of
          ["site"] -> "http://whclan.uk.to/"
          ["roster"] -> "http://goo.gl/IxfKm5"
          ["apl"] -> "https://altitudegame.com/forums/showthread.php?t=9733"
          _ -> "possible arguments: site roster apl"
     else return ()

counterB :: LoopBehavior Int
counterB =
  LoopBehavior { loopComputer = computer, loopInitState = 0 }
  where
    computer state (s, msg) =
      case mMsg msg of
        "!+1" -> return $ (state :: Int) + 1
        "!counter" -> (sendMsg s (fromJust $ mOrigin msg) $ B.pack (show state)) >> return state
        _ -> return state

events :: IO [IrcEvent]
events = 
  sequence 
    [ eventFromBehavior botsnackB
    , eventFromBehavior counterB 
    , eventFromBehavior linkB ]
