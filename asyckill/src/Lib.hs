module Lib where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception

someFunc :: IO ()
someFunc = test1

test1 :: IO ()
test1 = do
    mv <- newEmptyMVar  :: IO (MVar Int)
    lock <- newEmptyMVar
    tid <- forkFinallyMasked (worker mv) (\r -> do
        putStrLn ("Worker exited with: " ++ show r)
        putMVar lock ()
        )
    killThread tid
    _ <- takeMVar lock
    putStrLn "test done"

worker :: MVar Int -> IO ()
worker mv = m $ do
    ms <- getMaskingState
    uPrint ("worker start", ms)
    -- Point 1, an interruptible operation
    --   Interestingly, the deadlocking mvar read is still interrupted, even if
    --   m == uninterruptibleMask_, while the threadDelay works as expected (
    --   interrupted when m == id, not when == uninterruptibleMask_).
    i <- takeMVar mv
    -- threadDelay 1000000
    uPrint "worker end"
  where
    m = uninterruptibleMask_
    -- m = id

uPrint :: (Show a) => a -> IO ()
uPrint = uninterruptibleMask_ . print

forkFinallyMasked :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinallyMasked a athen =
    mask $ \restore ->
        forkIO $ try a >>= athen   -- Note 1: action is run masked! So gets killed at most at Point 1
