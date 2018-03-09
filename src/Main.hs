module Main where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Loops
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

data ThreadStatus =
    ThreadPreempted | ThreadSyscall
        deriving Show

type ThreadState = Coroutine (Yield ThreadStatus) IO String

loopyThread :: String -> ThreadState
loopyThread id = do
    lift $ putStrLn ("Init thread: " ++ id)
    whileM (return True) (do
        lift $ putStrLn ("In thread: " ++ id)
        yield ThreadPreempted)
    lift $ putStrLn ("End of thread: " ++ id)
    return ("Return: " ++ id)

-- Scheduler picks this to stop scheduling
dieThread :: ThreadState
dieThread = do
    lift $ putStrLn ("Dying...")
    return ("Returning dead")

startWith = [loopyThread "A", loopyThread "B"]

data SchedulerState = SchedulerState { threadStates :: [ThreadState]
                                     , lastPicked :: Int
                                     , pickIndex :: Int
                                     }

schedulerDo :: Show a => SchedulerState -> Yield a ThreadState -> (SchedulerState, ThreadState)
schedulerDo currentSchedulerState (Yield x newThreadState) =
    ((newSchedulerState), lift (print x) >> pickThread)
        where newSchedulerState :: SchedulerState
              newSchedulerState =
                (SchedulerState
                    ((threadStates currentSchedulerState) & element (lastPicked currentSchedulerState) .~ newThreadState)
                    (newPickIndex)
                    (succ $ pickIndex currentSchedulerState))
              newPickIndex :: Int
              newPickIndex = (pickIndex currentSchedulerState) `mod` (length $ threadStates currentSchedulerState)
              pickThread :: ThreadState
              pickThread = if (pickIndex currentSchedulerState < 10)
                            then (threadStates currentSchedulerState) !! newPickIndex
                            else dieThread


runScheduler :: ThreadState -> IO (SchedulerState, String)
runScheduler = foldRun schedulerDo (SchedulerState startWith 0 1)

main :: IO ()
main = do
    (resultState, resultString) <- runScheduler (startWith!!0)
    putStrLn ("Scheduler returned: " ++ resultString)
    putStrLn ("pickIndex: " ++ (show $ pickIndex resultState))
