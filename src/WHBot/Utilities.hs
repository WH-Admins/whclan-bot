module WHBot.Utilities
  ( doWithChan ) where

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad (void)

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

doWithChan :: (B.ByteString -> EventFunc) -> EventFunc
doWithChan f s msg = 
  chanFromMsg s msg >>= \maybeChan ->
    case maybeChan of
      Just channel -> f channel s msg
      Nothing -> return ()
