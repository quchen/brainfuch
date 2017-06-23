module Utilities where

import System.IO (hFlush, stdout)

flush :: IO ()
flush = hFlush stdout
