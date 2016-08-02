module Lib3 where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
  ( forever
  , unless
  )
import Control.Monad.Loops
import System.IO
import System.Random
import System.Environment
import qualified Control.Concurrent.Chan.Unagi.Bounded as U
import Data.Time.Clock
import Text.Printf

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS

type Coord = (Double, Double)

type Trace = [Coord]

type Queue = (U.InChan Trace, U.OutChan Trace)

-- | Represents a rectangle in the complex plane, bounded by a lower left
-- coordinate and an upper right coordinate.
data Plane
  = Plane { ll :: Coord, ur :: Coord }
  deriving (Show)

-- | Adds two coordinates.
(+.) :: Coord -> Coord -> Coord
(r1, i1) +. (r2, i2) = (r1 + r2, i1 + i2)

-- | Multiplies two coordinates.
(*.) :: Coord -> Coord -> Coord
(r1, i1) *. (r2, i2) = (r1*r2 - i1*i2, r1*i2 + r2*i1)

-- | Computes the square of a coordinate.
square :: Coord -> Coord
square (r, i) = (r*r - i*i, 2*r*i)

-- | Distance from origin to a given coordinate.
distFromOrigin :: Coord -> Double
distFromOrigin (r, i) = r*r + i*i

-- | A structure for passing data to the worker threads.
data WorkerData
  = WorkerData { wdMinIt :: Int
               , wdMaxIt :: Int
               , wdTraceQueue :: U.InChan Trace
                 -- ^ A queue of traces to be written to disk.
               }

-- | A structure for passing data to the manager thread.
data ManagerData
  = ManagerData { mdOutHandle :: Handle
                   -- ^ Handle to the output file.
                , mdNumTraces :: Integer
                  -- ^ Number of traces to gather.
                , mdTraceQueue :: U.OutChan Trace
                  -- ^ A queue of traces to be written to disk.
                }

-- | Encodes an entity to binary bytestring.
encode :: B.Binary a => a -> BS.ByteString
encode = B.encode

-- | Writes a lazy bytestring to file.
writeToFile :: Handle -> BS.ByteString -> IO ()
writeToFile = BS.hPut

mkManagerData :: Queue -> IO ManagerData
mkManagerData (inchan, outchan) =
  do let out_f = "test.out"
     out_h <- openBinaryFile out_f WriteMode
     let num_t = 1000
     return $ ManagerData { mdOutHandle = out_h
                          , mdNumTraces = num_t
                          , mdTraceQueue = outchan
                          }

mkWorkerData :: Queue -> IO WorkerData
mkWorkerData (inchan, outchan) =
  do let min_it =  1000
         max_it =  10000
     return $ WorkerData { wdMinIt = min_it
                         , wdMaxIt = max_it
                         , wdTraceQueue = inchan
                         }

-- | The actions to be performed by the manager thread.
runManager :: ManagerData -> IO ()
runManager m_data =
  do start <- getCurrentTime 
     execute start 0 (mdNumTraces m_data)
     return ()
  where 
    execute start count remaining | remaining <= 0 = return () 
    execute start count remaining =
        do item <- U.readChan $ mdTraceQueue m_data
           writeToFile (mdOutHandle m_data) (encode item)
           let count' = count+1
           if mod count' 10 == 0
             then do now <- getCurrentTime 
                     let delta = realToFrac (diffUTCTime now start) :: Double
                         elapsed = printf "%6.1f" delta
                     putStrLn $ elapsed ++ " - traces written: " ++ show count'
             else return ()
           execute start count' (remaining-1)

-- | The actions to be performed by a worker thread.
runWorker :: WorkerData -> IO ()
runWorker w_data =
  forever $
    do c <- randomCoord
       case computeTrace c (wdMinIt w_data) (wdMaxIt w_data) of
         Just t  -> U.writeChan (wdTraceQueue w_data) t
         Nothing -> return ()

{-
-- | Reads all values from a given 'TQueue'. If any other thread reads from the
-- same 'TQueue' during the execution of this function, then this function may
-- deadlock.
purgeTQueue :: Show a => TQueue a -> IO [a]
purgeTQueue q =
  whileJust (atomically $ tryReadTQueue q)
            (return . id)
-}

-- | Generates a random coordinate to trace.
randomCoord :: IO Coord
randomCoord =
  do x <- randomRIO (-2.102613, 1.200613)
     y <- randomRIO (-1.237710, 1.239710)
     return (x, y)

-- | Computes a trace, using the classical Mandelbrot function, for a given
-- coordinate and minimum and maximum iteration count. If the length of the
-- trace is less than the minimum iteration count, or exceeds the maximum
-- iteration count, 'Nothing' is returned.
computeTrace
  :: Coord
  -> Int
     -- ^ Minimum iteration count.
  -> Int
     -- ^ Maximum iteration count.
  -> Maybe Trace
computeTrace c0 min_it max_it =
  if isUsefulCoord c0
  then let step c = square c +. c0
           computeIt c it = if it < max_it
                            then computeIt (step c) (it + 1)
                            else it
           computeTr [] = error "computeTr: empty list"
           computeTr (c:cs) = if length cs < max_it
                              then computeTr (step c:(c:cs))
                              else (c:cs)
           num_it = computeIt c0 0
       in if num_it >= min_it && num_it <= max_it
          then Just $ reverse $ computeTr [c0]
          else Nothing
  else Nothing

-- | Checks if a given coordinate is useful by checking if it belongs in the
-- cardioid or period-2 bulb of the Mandelbrot.
isUsefulCoord :: Coord -> Bool
isUsefulCoord (x, y) =
  let t1 = x - 1/4
      p = sqrt (t1*t1 + y*y)
      is_in_cardioid = x < p - 2*p*p + 1/4
      t2 = x + 1
      is_in_bulb = t2*t2 + y*y < 1/16
  in not is_in_cardioid && not is_in_bulb

main :: IO ()
main =
  do args <- getArgs
     case args of
       (a:_)   -> setStdGen (mkStdGen (read a))
       _       -> error "bad usage"
     t_queue <- newTQueueIO
     queue <- U.newChan 1000
     m_data <- mkManagerData  queue
     w_data <- mkWorkerData queue
     let num_workers = 1
     workers <- mapM async (replicate num_workers (runWorker w_data))
     runManager m_data
     _ <- mapM cancel workers
     _ <- mapM waitCatch workers
     putStrLn "Tracing finished" 
