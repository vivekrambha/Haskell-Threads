module ExceptionHandler where

import Control.Exception
import System.IO.Error (isDoesNotExistError)

-- | Handle the exception in case the file does not exist.
handleExists :: IOException -> IO ()
handleExists e
  | isDoesNotExistError e = return ()
  | otherwise = throwIO e
