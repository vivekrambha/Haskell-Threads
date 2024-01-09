module Message where

-- | Represents a message in the social network.
data Message = Message
  { fromUser :: String  
  , content :: String 
  }

instance Show Message where
  show (Message from content) = from ++ ": " ++ content
