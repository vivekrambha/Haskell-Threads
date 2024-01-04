{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.Monad
import System.Random
import System.IO
import System.Directory
import Control.Exception
import System.IO.Error (isDoesNotExistError)

-- | Represents a user in the social network.
data User = User
  { username :: String
  , receivedMessages :: MVar [Message] 
  } deriving (Eq)

-- | Represents a message in the social network.
data Message = Message
  { fromUser :: String  
  , content :: String 
  }

instance Show Message where
  show (Message from content) = from ++ ": " ++ content

-- | List of usernames based on planet names.
planetNames :: [String]
planetNames = ["Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto", "Eris"]

-- | Creates a new user with the given username.
createUser :: String -> IO User
createUser name = do
  mbox <- newMVar []
  return $ User name mbox

-- | Sends a message from one user to another and logs the message to a common file.
sendMessage :: MVar () -> User -> User -> Message -> IO ()
sendMessage fileLock from to message = do
  let fileName = "all_messages.txt"
  let formattedMessage = "------------\n" ++
                         "Message received from: " ++ username from ++
                         "\nTo chat: " ++ username to ++
                         "\n" ++ content message ++ "\n\n"
  withMVar fileLock $ \_ -> appendFile fileName formattedMessage

  -- Update the recipient's received messages
  modifyMVar_ (receivedMessages to) $ \messages -> return (message : messages)

-- | Simulates the activity of a user.
userActivity :: MVar () -> [User] -> User -> IO ()
userActivity fileLock users currentUser = replicateM_ 100 $ do
  threadDelay =<< randomRIO (100000, 500000)
  
  -- Filter out the current user from the list of potential recipients
  let potentialRecipients = filter (/= currentUser) users

  -- Pick a random user from the filtered list to send a message to
  recipientIndex <- randomRIO (0, length potentialRecipients - 1)
  let recipient = potentialRecipients !! recipientIndex
  let sender = username currentUser

  -- Create and send a message
  let messageContent = "Hello from " ++ sender
  let message = Message sender messageContent
  sendMessage fileLock currentUser recipient message

-- | The main function that starts the social network simulation.
main :: IO ()
main = do
  -- Remove existing all_messages.txt file if it exists
  removeFile "all_messages.txt" `catch` handleExists
  fileLock <- newMVar ()
  
  -- Create users using the list of planet names
  users <- mapM createUser planetNames

  -- Spawn threads for each user
  mapM_ (forkIO . userActivity fileLock users) users

  -- Wait for all messages to be sent
  threadDelay 10000000

  -- Output the final count of messages each user received
  forM_ users $ \user -> do
    messages <- readMVar (receivedMessages user)
    putStrLn $ username user ++ " received " ++ show (length messages) ++ " messages."

-- | Handle the exception in case the file does not exist.
handleExists :: IOException -> IO ()
handleExists e
  | isDoesNotExistError e = return ()
  | otherwise = throwIO e
