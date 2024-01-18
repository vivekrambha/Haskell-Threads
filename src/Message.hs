module Message where

-- | structure and data types for a message and also how it displays the message.
data Message = Message
  { fromUser :: String  
  , content :: String 
  }

instance Show Message where
  show (Message from content) = from ++ ": " ++ content
