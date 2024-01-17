module UserAnalytics where

import Control.Concurrent
import User (User(..))
import Message (Message(..))
import System.Random (randomRIO)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- | Retrieves the user who has received the most messages.
userWithMostMessages :: [User] -> IO User
userWithMostMessages users = do
  userMessageCounts <- mapM (\u -> fmap length (readMVar $ messages u)) users
  let userCountPairs = zip users userMessageCounts
  return $ fst $ maximumBy (comparing snd) userCountPairs

-- | Displays information about the user with the most messages.
displayTopUser :: User -> IO ()
displayTopUser user = do
  userMsgs <- readMVar (messages user)
  putStrLn $ "User with most messages: " ++ name user
  putStrLn $ "Number of messages received: " ++ show (length userMsgs)
  putStrLn $ "-----------------------------"

-- | Customizes a message for a user before sending.
customizeMessage :: User -> IO Message
customizeMessage sender = do
  let senderName = name sender
  randomIndex <- randomRIO (0, length customMessages - 1)
  let messageContent = customMessages !! randomIndex
  return $ Message senderName messageContent

customMessages :: [String]
customMessages = [ "Welcome to the cosmos! Let's explore the stars together."
  , "Hello from the event horizon! Haskell's gravity is strong."
  , "Keep orbiting and stay in your gravity well!"
  , "Stay safe within the asteroid belt!"
  , "Happy coding! May your code be as scalable as the universe."
  ]
