module User where

import Control.Concurrent
import Message (Message)

-- | Structure and Data type of a User
data User = User
  { name :: String
  , messages :: MVar [Message] 
  } deriving (Eq)
