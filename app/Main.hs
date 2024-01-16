module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import System.IO
import System.Directory (removeFile)
import Control.Exception (catch)
import User (User(..))
import Message (Message(..))
import Utils (planetNames)
import ExceptionHandler (handleExists)

-- | Creates a new user with the given name.
createUser :: String -> IO User
createUser uname = do
  mbox <- newMVar []
  return $ User uname mbox

-- | Sends a message from one user to another and logs the message to a common file.
sendMessage :: MVar () -> MVar Int -> User -> User -> Message -> IO ()
sendMessage fileLock messageCount from to message = do
  currentCount <- takeMVar messageCount
  if currentCount < 100 then do
    let fileName = "all_messages.txt"
    let formattedMessage = "------------\n" ++
                          "Message received from: " ++ name from ++
                          "\nTo chat: " ++ name to ++
                          "\n" ++ content message ++ "\n\n"
    withMVar fileLock $ \_ -> appendFile fileName formattedMessage
    modifyMVar_ (messages to) $ \msgs -> return (message : msgs)
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
    let sender = name currentUser
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
    userMessages <- readMVar (messages user)
    putStrLn $ name user ++ " received " ++ show (length userMessages) ++ " messages."
