module ExceptionHandler where

import Control.Exception
import System.IO.Error (isDoesNotExistError)

-- | Exception handling for file if it doesn't exist
handleExists :: IOException -> IO ()
handleExists e
  | isDoesNotExistError e = return ()
  | otherwise = throwIO e
