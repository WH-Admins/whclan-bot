module WHBot 
  ( events ) where

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad 

import System.Random

import WHBot.Behaviors

-- botsnackB
botsnackB :: ChannelBehavior
botsnackB = ChannelBehavior botsnackComputer

botsnackComputer :: B.ByteString -> EventTrigger -> IO ()
botsnackComputer channel trigger =
  when (head contentWords == "!botsnack") $ replyMessage trigger "delicious, mkay"
  where contentWords = getContentWords trigger

-- slapB
slapB :: ChannelBehavior
slapB = ChannelBehavior slapComputer

slapWords :: String-> [String]
slapWords victim =
  map (\x -> "*" ++ x ++ "*") $
    [ "attacks " ++ victim
    , "slaps " ++ victim ++ " with a dead fish"
    , "blows up" ++ victim
    , "discusses politics with " ++ victim ]

slapComputer :: B.ByteString -> EventTrigger -> IO ()
slapComputer channel trigger =
  when (head contentWords == "!slap") $ 
    case tail contentWords of
      [victim] -> 
        let choices = slapWords victim in
          do
            a <- randomRIO (0, ((length choices) - 1))
            replyMessage trigger (B.pack (choices !! a))
      _ -> return ()
  where contentWords = getContentWords trigger

-- linkB
linkB :: SimpleBehavior
linkB = SimpleBehavior linkComputer

linkHelp :: B.ByteString
linkHelp = "This utility provides various links to clan-related things. Acceptable arguments: site, roster, apl."

linkComputer :: EventTrigger -> IO ()
linkComputer trigger =
  when (head contentWords == "!link") $
    replyMessage trigger $
      case tail contentWords of
        ["site"] -> "http://whclan.uk.to/"
        ["roster"] -> "http://goo.gl/IxfKm5"
        ["apl"] -> "https://altitudegame.com/forums/showthread.php?t=9733"
        _ -> linkHelp
  where contentWords = getContentWords trigger

-- counterB
counterB :: LoopBehavior Int
counterB = LoopBehavior { loopComputer = counterComputer, loopInitState = 0 }

counterHelp :: B.ByteString
counterHelp =
  "This is a simple counter device. The 'add' argument increments the counter by 1, the 'read' argument prints the current counter state, and the 'reset' argument resets the counter to 0"

counterComputer :: Int -> EventTrigger -> IO Int
counterComputer state trigger =
  if (head contentWords == "!counter") then
    case tail contentWords of
      ["add"] -> (>> return (state + 1)) $ replyMessage trigger "+1!"
      ["read"] -> (>> return state) $ replyMessage trigger (B.pack $ show state)
      ["reset"] -> (>> return 0) $ replyMessage trigger "counter reset to 0"
      _ -> (>> return state) $ replyMessage trigger counterHelp
  else return state
  where contentWords = getContentWords trigger

-- helpB 
helpB :: SimpleBehavior
helpB = SimpleBehavior helpComputer

helpComputer :: EventTrigger -> IO ()
helpComputer trigger =
  when ((head contentWords) == "!help") $
    case tail contentWords of
      ["credits"] -> replyMessage trigger "Programmed by MagneticDuck in the wonderful language of Haskell."
      _ -> replyMessage trigger "This is a simple IRC bot with a few commands that can be of use to the #wreaking channel. Currently, supported commands are: !help !link !counter !botsnack"
  where
    contentWords = getContentWords trigger

events :: IO [IrcEvent]
events = 
  sequence 
    [ eventFromBehavior botsnackB
    , eventFromBehavior counterB 
    , eventFromBehavior linkB 
    , eventFromBehavior helpB 
    , eventFromBehavior slapB ]
