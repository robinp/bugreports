{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Data.IORef

main :: IO ()
main = quickCheck prop_async_in_cleanup

forM = flip mapM

prop_async_in_cleanup :: Int -> Property
prop_async_in_cleanup n = ioProperty $ do
    cnt <- newIORef 0
    tids <- forM [0..n] $ \i -> (mask_ . forkIO) (uninterruptibleMask $ \r -> do
        ms <- getMaskingState
        r (print ("worker", i, ms))
        -- Note: allowInterrupt behavior changed in 7.10.3 -> 8.0 (does not allow in Uninterruptible state).
        me <- try (r (allowInterrupt >> (print ("point", i))))
        case me of
            Left (e :: SomeException) -> do
                print ("ex", i, e)
                atomicModifyIORef' cnt (\c -> (c+1, ()))
            Right _ -> return $! ())
    mapM_ killThread tids
    fmap (== 0) (readIORef cnt)
