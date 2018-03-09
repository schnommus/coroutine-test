module Main where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Loops
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

data ThreadStatus =
    ThreadPreempted | ThreadSyscall
        deriving Show

type ThreadContext = Coroutine (Yield ThreadStatus) IO String

loopyThread :: String -> ThreadContext
loopyThread id = do
    lift $ putStrLn ("Init thread: " ++ id)
    whileM (return True) (do
        lift $ putStrLn ("In thread: " ++ id)
        yield ThreadPreempted)
    lift $ putStrLn ("End of thread: " ++ id)
    return ("Return: " ++ id)

-- Kernel picks this to stop running
dieThread :: ThreadContext
dieThread = do
    lift $ putStrLn ("Dying...")
    return ("Returning dead")

startWith = [loopyThread "A", loopyThread "B"]

data KernelState = KernelState { threadStates :: [ThreadContext]
                               , lastPicked   :: Int
                               , timeStamp    :: Int
                               }

kernelDo :: Show a => KernelState -> Yield a ThreadContext -> (KernelState, ThreadContext)
kernelDo ks (Yield x newThreadContext) =
    (ks', lift (putStrLn (show x ++ " @T=" ++ show (timeStamp ks))) >> pickThread)
        where ks' = KernelState
                        ((threadStates ks) & element (lastPicked ks) .~ newThreadContext)
                        (newPickIndex)
                        (timeStamp ks + 1)

              newPickIndex :: Int
              newPickIndex = (lastPicked ks + 1) `mod` (length $ threadStates ks)

              pickThread :: ThreadContext
              pickThread = if (timeStamp ks < 10)
                            then (threadStates ks) !! newPickIndex
                            else dieThread


runKernel :: ThreadContext -> IO (KernelState, String)
runKernel = foldRun kernelDo (KernelState startWith 0 0)

main :: IO ()
main = do
    (resultState, resultString) <- runKernel (startWith!!0)
    putStrLn ("Kernel returned: " ++ resultString)
    putStrLn ("timeStamp: " ++ (show $ timeStamp resultState))
