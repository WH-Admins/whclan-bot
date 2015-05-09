module WHBot 
  ( jokeFile, events ) where

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Monad 

import Paths_whclan_bot

import WHBot.Behaviors

-- fixme
jokeFile, linkFile :: IO String
jokeFile = getDataFileName "one-liners"
linkFile = getDataFileName "links"

-- tickleB
tickleB :: ChannelBehavior
tickleB = ChannelBehavior tickleComputer

tickleComputer :: B.ByteString -> EventTrigger -> IO ()
tickleComputer channel trigger =
  when (head contentWords == "!tickle") $ 
    replyMessage trigger . B.pack $ 
      "I've fking had it with you and your fkin tickles, " ++ (B.unpack . fromJust $ mNick (snd trigger))
  where contentWords = getContentWords trigger

-- botsnackB
botsnackB :: ChannelBehavior
botsnackB = ChannelBehavior botsnackComputer

botsnackComputer :: B.ByteString -> EventTrigger -> IO ()
botsnackComputer channel trigger =
  when (head contentWords == "!botsnack") $ replyMessage trigger "delicious, mkay"
  where contentWords = getContentWords trigger

-- jokeB
jokeB :: SimpleBehavior
jokeB = SimpleBehavior jokeComputer

jokeComputer :: EventTrigger -> IO ()
jokeComputer trigger = 
  when (head contentWords == "!joke") $ 
    do
      jokes <- (fmap lines . readFile) =<< jokeFile
      replyMessage trigger =<< (fmap B.pack $ randomChoice jokes)
  where
    contentWords = getContentWords trigger

-- slapB
slapB :: ChannelBehavior
slapB = ChannelBehavior slapComputer

slapWords :: String-> [String]
slapWords victim =
  map (\x -> "*" ++ x ++ "*") $
    [ "attacks " ++ victim
    , "slaps " ++ victim ++ " with a dead fish"
    , "slaps " ++ victim ++ " with a potatoe"
    , "slaps " ++ victim ++ " with, uh.. with.. a um. nah."
    , "asks " ++ victim ++ " if everything is alright"
    , "asks " ++ victim ++ " if everything is alright, and then slaps them with a potatoe"
    , "asks " ++ victim ++ " if everything is alright, and then blows them up with a potatoe"
    , "asks " ++ victim ++ " if they want more tea"
    , "blows up " ++ victim
    , "blows up " ++ victim ++ " with a potatoe"
    , "blows up " ++ victim ++ " with a dead fish"
    , "throws a combustible potatoe at " ++ victim
    , "throws a potatoe at " ++ victim
    , "lectures " ++ victim ++ " on the many merits of functional-reactive programming"
    , "discusses politics with " ++ victim ]

slapHelp :: B.ByteString
slapHelp =
  "This extremely useful utility lets you slap people when they deserve it / when you're in a bad mood. Can slap multiple people."

slapComputer :: B.ByteString -> EventTrigger -> IO ()
slapComputer channel trigger =
  when (head contentWords == "!slap") $ 
    case tail contentWords of
      victims@(_:_) -> 
        (flip mapM_ victims) $ \victim ->
          (replyMessage trigger . B.pack) =<< randomChoice (slapWords victim)
      _ -> replyMessage trigger slapHelp
  where contentWords = getContentWords trigger

-- linkB
linkB :: SimpleBehavior
linkB = SimpleBehavior linkComputer

linkFileData :: IO [(String, String)]
linkFileData = 
  do
    linkLines <- (fmap lines . readFile) =<< linkFile
    return $ zip (filter pred linkLines) (filter (not . pred) linkLines)
  where
    pred = (/= ' ') . head

linkHelp :: IO B.ByteString
linkHelp = linkFileData >>= return . B.pack . ("Currenly available links: " ++) . unwords . map fst 

linkComputer :: EventTrigger -> IO ()
linkComputer trigger =
  when (head contentWords == "!link") $
    case tail contentWords of
      [name] ->
        linkFileData >>= \mydata -> 
          case filter ((== name) . fst) mydata of
            ((_, mylink):_) -> replyMessage trigger . B.pack $ name ++ " -> " ++ mylink
            _ -> replyMessage trigger . B.pack $ "there appears to be no link with the name " ++ name
      _ -> replyMessage trigger =<< linkHelp
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
      _ -> replyMessage trigger "This is a simple IRC bot with a few commands that can be of use to the #wreaking channel. Currently, supported commands are: !help !link !counter !botsnack !slap !joke !tickle"
  where
    contentWords = getContentWords trigger

events :: IO [IrcEvent]
events = 
  sequence 
    [ eventFromBehavior helpB 
    , eventFromBehavior linkB 
    , eventFromBehavior counterB 
    , eventFromBehavior botsnackB
    , eventFromBehavior slapB 
    , eventFromBehavior tickleB ]
