{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import qualified Prometheus as P

main :: IO ()
main = do
    m <- P.registerIO (P.summary (P.Info "metric" "desc") P.defaultQuantiles)
    forever (meter m)

meter :: P.Metric P.Summary -> IO ()
meter m = do
    let rr = [0..7]
    counter <- newMVar (length rr)
    block <- newEmptyMVar
    mapM_ (const $ cfork counter block action) rr
    void $ takeMVar block
  where
    cfork c b a = forkIO $ do
        a
        done <- modifyMVar c (\v -> let n = v-1 in print n >> return (n, n==0))
        when done $
            putStrLn "Unblocking" >> putMVar b ()
    -- Bug here. The concurrent observations lead to some mess in the RTS.
    -- See README.md for a detailed explanation.
    action :: IO ()
    action = flip P.observeDuration m $ threadDelay 100
