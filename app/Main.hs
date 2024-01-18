import Control.Concurrent
import Control.Monad
import System.Random
import System.Directory (removeFile)
import Control.Exception (catch)
import User (User(..))
import Message (Message(..))
import Utils (planetNames)
import ExceptionHandler (handleExists)
import UserAnalytics (userWithMostMessages, displayTopUser, customizeMessage)

-- | Creating new user
createUser :: String -> IO User
createUser uname = do
  mbox <- newMVar []
  return $ User uname mbox

-- | Sends a message from one user to another and stores in all_messages.txt file.
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

-- | Simulating the activity of the user
userActivity :: MVar () -> MVar Int -> [User] -> User -> IO ()
userActivity fileLock messageCount users currentUser = forever $ do
  threadDelay =<< randomRIO (100000, 500000)
  currentCount <- readMVar messageCount
  when (currentCount < 100) $ do
    let potentialRecipients = filter (/= currentUser) users
    recipientIndex <- randomRIO (0, length potentialRecipients - 1)
    let recipient = potentialRecipients !! recipientIndex
    message <- customizeMessage currentUser
    sendMessage fileLock messageCount currentUser recipient message

-- | Main function Start of the application
main :: IO ()
main = do
  removeFile "all_messages.txt" `catch` handleExists
  fileLock <- newMVar ()
  messageCount <- newMVar 0

  users <- mapM createUser planetNames
  mapM_ (forkIO . userActivity fileLock messageCount users) users

 
  threadDelay 10000000
  finalCount <- readMVar messageCount
  putStrLn $ "------------------------"
  putStrLn $ "Total messages sent: " ++ show finalCount
  putStrLn $ "------------------------"

-- to get user with most messages 
  topUser <- userWithMostMessages users
  displayTopUser topUser

  -- Output
  forM_ users $ \user -> do
    userMessages <- readMVar (messages user)
    putStrLn $ name user ++ " received " ++ show (length userMessages) ++ " messages."
