module Main (main) where

import qualified Coup
-- import System.Remote.Monitoring

main :: IO ()
main = Coup.main
  -- ekg <- forkServer "localhost" 8000
