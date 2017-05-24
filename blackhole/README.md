=== Compiler & OS

GHC 7.10.3, 8.0.1, 8.0.2 (didn't check others).

Linux: 4.10.13-1-ARCH #1 SMP PREEMPT x86_64 GNU/Linux 4-core

=== What does the code do?

TLDR endlessly forks a batch of 8 threads, and waits for them to finish. Each
thread calls `observeDuration` on an irrelevant IO action. `observeDuration`
measures the time, then updates some data structures in `STM` context in a
`TVar`.

After a short while (usually within 1 minute), the program aborts with
`<<loop>>`. See below for a more detailed investigation.

=== To run

  stack install
  # Restart if doesn't terminate in a minute.
  loop-exe +RTS -N4 -Ds > debug 2> debug.out ; beep

=== Observe

`debug` shows the metering batches run for a while, then get stuck.

`debug.out` contains `<<loop>>`.

My debug log reading fu is poor, but in less cleaned-up versions of the program
it was more trivial to see that two threads get blocked on each others
blackhole.

In the current log there is mentioning of blackholes, but also MVars, and I
don't see what's going on.

=== Tracking down

When built with profiling (remove `--eventlog --debug` from cabal file, then
`stack clean` then `stack build --profile`), and running with `+RTS -N4 -xc`
the following stack traces appear:

		*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
			Main.meter,
			called from Main.main
		*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
			Prometheus.Metric.Summary.observe,
			called from Prometheus.Metric.Summary.observeDuration,
			called from Main.meter.action,
			called from Main.meter.cfork,
			called from Main.meter,
			called from Main.main
		*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
			Prometheus.Metric.Summary.observe,
			called from Prometheus.Metric.Summary.observeDuration,
			called from Main.meter.action,
			called from Main.meter.cfork,
			called from Main.meter,
			called from Main.main
		*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
			*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
			Main.meter,
			called from Main.mainPrometheus.Metric.Summary.observe,
			called from Prometheus.Metric.Summary.observeDuration,
			called from Main.meter.action,
			called from Main.meter.cfork,
			called from Main.meter,
			called from Main.main

		*** Exception (reporting due to +RTS -xc): (IND_STATIC), stack trace: 
			Prometheus.Metric.Summary.observe,
			called from Prometheus.Metric.Summary.observeDuration,
			called from Main.meter.action,
			called from Main.meter.cfork,
			called from Main.meter,
			called from Main.main
		loop-exe: loop-exe: <<loop>>
		<<loop>>
		loop-exe: thread blocked indefinitely in an MVar operation

An other stack trace from an other run as bonus (these two are representative):

		*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
			Prometheus.Metric.Summary.invariant,
			called from Prometheus.Metric.Summary.compress,
			called from Prometheus.Metric.Summary.observe,
			called from Prometheus.Metric.Summary.observeDuration,
			called from Main.meter.action,
			called from Main.meter.cfork,
			called from Main.meter,
			called from Main.main
		*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
			Prometheus.Metric.Summary.invariant,
			called from Prometheus.Metric.Summary.compress,
			called from Prometheus.Metric.Summary.observe,
			called from Prometheus.Metric.Summary.observeDuration,
			called from Main.meter.action,
			called from Main.meter.cfork,
			called from Main.meter,
			called from Main.main
		loop-exe: <<loop>>
		*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
			Main.meter,
			called from Main.main
		*** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
			Main.meter,
			called from Main.main
		loop-exe: thread blocked indefinitely in an MVar operation

I tried to even simplify by removing `prometheus-client` dep, but it seems that
the kind of computation done in Prometheus.Metric.Summary.observe, namely
putting a lazy computation inside a data type stored in a TVar, are needed to
trigger the blackholing. When I tried to simplify those operations, or bang
the remaining few fields of the data type, the error didn't reproduce (at least
couldn't reproduce as quick as it usually does).

The relevant pieces of code from prometheus-client's P.M.Summary module. Note:
I manually replaced the `MonadMonitor` constraint with plain IO in that call
chain, but it didn't have much effect.

		data Estimator = Estimator {
				estCount      :: !Int64
		,   estSum        :: !Double
		,   estQuantiles  :: [Quantile]
		,   estItems      :: [Item]
		} deriving (Show)

		newtype Summary = MkSummary (STM.TVar Estimator)

		observeDuration :: IO a -> Metric Summary -> IO a
		observeDuration io metric = do
				start  <- getCurrentTime
				result <- io
				end    <- getCurrentTime
				let dt = fromRational $ toRational $ end `diffUTCTime` start
				withSummary metric dt
				return result

		observe :: MonadMonitor m => Double -> Metric Summary -> m ()
		observe v s = withSummary s (insert v)

		withSummary :: MonadMonitor m
								=> Metric Summary -> (Estimator -> Estimator) -> m ()
		withSummary (Metric {handle = MkSummary valueTVar}) f =
				doIO $ STM.atomically $ do
						STM.modifyTVar' valueTVar compress
						STM.modifyTVar' valueTVar f


		insert :: Double -> Estimator -> Estimator

		compress :: Estimator -> Estimator

I checked `insert` and `compress` and they don't seem to be able to loop in
any edge case, so this bug is likely an RTS thing.

=== Related tickets I found:

https://ghc.haskell.org/trac/ghc/ticket/10218
		GHC creates incorrect code which throws <<loop>>

https://ghc.haskell.org/trac/ghc/ticket/10414 :
    Buggy behavior with threaded runtime (-N1 working, -N2 getting into <<loop>>)

