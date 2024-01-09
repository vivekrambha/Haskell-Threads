module User where

import Control.Concurrent
import Message (Message)

-- | Represents a user in the social network.
data User = User
  { name :: String
  , messages :: MVar [Message] 
  } deriving (Eq)
