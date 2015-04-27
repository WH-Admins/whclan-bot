module WHBot.Behaviors
  ( replyMessage, getContentWords
  , randomChoice
  , EventTrigger

  , eventFromBehavior 

  , SimpleBehavior(..)
  , ChannelBehavior(..)
  , LoopBehavior(..)) where

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad (void)
import Data.IORef

import System.Random

chanFromMsg :: MIrc -> IrcMessage -> IO (Maybe B.ByteString)
chanFromMsg s msg = 
  case mChan msg of
    Just channel -> 
      case B.unpack channel of
        ('#':_) -> return $ Just channel
        _ -> failReturn 
    Nothing -> failReturn
  where
    failReturn = return Nothing

randomChoice :: [a] -> IO a
randomChoice xs =
  do
    a <- randomRIO (0, (length xs) - 1)
    return $ xs !! a

type EventTrigger = (MIrc, IrcMessage)

replyMessage :: EventTrigger -> B.ByteString -> IO ()
replyMessage (s, msg) = sendMsg s (fromJust $ mOrigin msg) 

getContentWords :: EventTrigger -> [String]
getContentWords = words . B.unpack . mMsg . snd

-- class definition
class IrcBehavior behavior where
  eventFromBehavior :: behavior -> IO IrcEvent

-- SimpleBehavior
data SimpleBehavior =
  SimpleBehavior { simpleComputer :: EventTrigger -> IO () }

instance IrcBehavior SimpleBehavior where
  eventFromBehavior (SimpleBehavior computer) =
    return $ Privmsg (curry computer)

-- ChannelBehavior
data ChannelBehavior =
  ChannelBehavior { channelComputer :: B.ByteString -> EventTrigger -> IO () }

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

