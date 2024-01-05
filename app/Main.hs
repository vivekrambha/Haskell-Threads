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
sendMessage :: MVar () -> MVar Int -> User -> User -> Message -> IO ()
sendMessage fileLock messageCount from to message = do
  currentCount <- takeMVar messageCount
  if currentCount < 100 then do
    let fileName = "all_messages.txt"
    let formattedMessage = "------------\n" ++
                          "Message received from: " ++ username from ++
                          "\nTo chat: " ++ username to ++
                          "\n" ++ content message ++ "\n\n"
    withMVar fileLock $ \_ -> appendFile fileName formattedMessage
    modifyMVar_ (receivedMessages to) $ \messages -> return (message : messages)
    putMVar messageCount (currentCount + 1)
  else
    putMVar messageCount currentCount

-- | Simulates the activity of a user.
userActivity :: MVar () -> MVar Int -> [User] -> User -> IO ()
userActivity fileLock messageCount users currentUser = forever $ do
  threadDelay =<< randomRIO (100000, 500000)
  currentCount <- readMVar messageCount
  when (currentCount < 100) $ do
    let potentialRecipients = filter (/= currentUser) users
    recipientIndex <- randomRIO (0, length potentialRecipients - 1)
    let recipient = potentialRecipients !! recipientIndex
    let sender = username currentUser
    let messageContent = "Hello from " ++ sender
    let message = Message sender messageContent
    sendMessage fileLock messageCount currentUser recipient message

-- | The main function that starts the social network simulation.
main :: IO ()
main = do
  removeFile "all_messages.txt" `catch` handleExists
  fileLock <- newMVar ()
  messageCount <- newMVar 0

  users <- mapM createUser planetNames
  mapM_ (forkIO . userActivity fileLock messageCount users) users

  -- Wait for all messages to be sent
  threadDelay 10000000
  finalCount <- readMVar messageCount
  putStrLn $ "Total messages sent: " ++ show finalCount

  -- Output the final count of messages each user received
  forM_ users $ \user -> do
    messages <- readMVar (receivedMessages user)
    putStrLn $ username user ++ " received " ++ show (length messages) ++ " messages."

-- | Handle the exception in case the file does not exist.
handleExists :: IOException -> IO ()
handleExists e
  | isDoesNotExistError e = return ()
  | otherwise = throwIO e
