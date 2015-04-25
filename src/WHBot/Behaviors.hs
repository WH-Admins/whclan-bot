module WHBot.Behaviors
  ( EventTrigger

  , eventFromBehavior 

  , ChannelBehavior(..)
  
  , LoopBehavior(..)) where

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad (void)
import Data.IORef

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

type EventTrigger = (MIrc, IrcMessage)

class IrcBehavior behavior where
  eventFromBehavior :: behavior -> IO IrcEvent

data ChannelBehavior =
  ChannelBehavior { channelComputer :: B.ByteString -> EventTrigger -> IO () }

-- IrcBehavior
instance IrcBehavior ChannelBehavior where
  eventFromBehavior (ChannelBehavior computer) =
    let 
      simpleFunc s msg =
        chanFromMsg s msg >>= \maybeChan ->
          case maybeChan of
            Just channel -> computer channel (s, msg) 
            Nothing -> return ()
    in
      return $ Privmsg simpleFunc

-- LoopBehavior
data LoopBehavior state = 
  LoopBehavior 
    { loopComputer :: state -> EventTrigger -> IO state 
    , loopInitState :: state }

instance IrcBehavior (LoopBehavior state) where
  eventFromBehavior (LoopBehavior loop initState) =
    do
      a <- newIORef initState
      return . Privmsg $
        (\s msg -> 
          do
            val <- readIORef a
            loop val (s, msg) >>= writeIORef a )

