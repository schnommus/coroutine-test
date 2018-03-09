module Main where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Loops
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

data TaskStatus =
    TaskPreempted | TaskSyscall
        deriving Show

type TaskState = Coroutine (Yield TaskStatus) IO String

task :: String -> TaskState
task id = do
    lift $ putStrLn ("Init task: " ++ id)
    whileM (return True) (do
        lift $ putStrLn ("In task: " ++ id)
        yield TaskPreempted)
    lift $ putStrLn ("End of task: " ++ id)
    return ("Return: " ++ id)

-- Scheduler picks this to stop scheduling
dieTask :: TaskState
dieTask = do
    lift $ putStrLn ("Dying...")
    return ("Returning dead")

startWith = [task "A", task "B", task "cooltask"]

data SchedulerState = SchedulerState { taskStates :: [TaskState]
                                     , lastPicked :: Int
                                     , pickIndex :: Int
                                     }

schedulerDo :: Show a => SchedulerState -> Yield a TaskState -> (SchedulerState, TaskState)
schedulerDo currentSchedulerState (Yield x newTaskState) =
    ((newSchedulerState), lift (print x) >> pickTask)
        where newSchedulerState :: SchedulerState
              newSchedulerState =
                (SchedulerState
                    ((taskStates currentSchedulerState) & element (lastPicked currentSchedulerState) .~ newTaskState)
                    (newPickIndex)
                    (succ $ pickIndex currentSchedulerState))
              newPickIndex :: Int
              newPickIndex = (pickIndex currentSchedulerState) `mod` (length $ taskStates currentSchedulerState)
              pickTask :: TaskState
              pickTask = if (pickIndex currentSchedulerState < 10)
                            then (taskStates currentSchedulerState) !! newPickIndex
                            else dieTask


runScheduler :: TaskState -> IO (SchedulerState, String)
runScheduler = foldRun schedulerDo (SchedulerState startWith 0 1)

main :: IO ()
main = do
    (resultState, resultString) <- runScheduler (startWith!!0)
    putStrLn ("Scheduler returned: " ++ resultString)
    putStrLn ("pickIndex: " ++ (show $ pickIndex resultState))
