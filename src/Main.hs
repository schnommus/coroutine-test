{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Loops
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Exception.Base
import Debug.Trace

import Data.Array
import Data.Word8

type Priority = Word8

data NTFN
        = IdleNtfn
        | ActiveNtfn { ntfnMsgIdentifier :: Word }
        | WaitingNtfn { ntfnQueue :: [TCB] }
    deriving Show

data Notification = Notification {
    ntfnObj :: NTFN,
    ntfnBoundTCB :: Maybe TCB }
        deriving Show

data Endpoint
        = RecvEP { epQueue :: [TCB] }
        | IdleEP
        | SendEP { epQueue :: [TCB] }
        deriving (Show)

data ThreadState
    = BlockedOnReceive {
        blockingObject :: Endpoint }
    | BlockedOnReply
    | BlockedOnNotification {
        waitingOnNotification :: Notification }
    | Running
    | Inactive
    | IdleThreadState
    | BlockedOnSend {
        blockingObject :: Endpoint }
    | Restart
    deriving (Show)

data Syscall
        = SysReplyRecv
        | SysSend
        | SysNBSend
        | SysRecv
        | SysReply
        | SysYield
        | SysNBRecv
    deriving (Show, Eq)

data ThreadStatus =
    ThreadPreempted | ThreadSyscall Syscall
        deriving (Show, Eq)

type ThreadContext = Coroutine (Yield ThreadStatus) IO String

instance Show ThreadContext where
    show tc = "<ThreadContext>"

data TCB = TCB { tcbState :: ThreadState
               , tcbPriority :: Priority
               , tcbQueued :: Bool
               , tcbTimeSlice :: Int
               , tcbBoundNotification :: Maybe Notification
               , tcbContext :: ThreadContext
               , tcbId :: Int
               } deriving (Show)

instance Eq TCB where
    a == b = tcbId a == tcbId b

data SchedulerAction
    = ResumeCurrentThread
    | ChooseNewThread
    | SwitchToThread TCB
    deriving (Show)

type ReadyQueue = [TCB]

data KernelState = KernelState { ksReadyQueues :: Array Priority ReadyQueue
                               , ksCurThread :: TCB
                               , ksIdleThread :: TCB
                               , ksSchedulerAction :: SchedulerAction
                               , ksTimeStamp    :: Int
                               } deriving (Show)

type Kernel = State KernelState


getCurThread :: Kernel (TCB)
getCurThread = gets ksCurThread

setCurThread :: TCB -> Kernel ()
setCurThread tptr = modify (\ks -> ks { ksCurThread = tptr })

getIdleThread :: Kernel (TCB)
getIdleThread = gets ksIdleThread

setIdleThread :: TCB -> Kernel ()
setIdleThread tptr = modify (\ks -> ks { ksIdleThread = tptr })

getQueue :: Priority -> Kernel ReadyQueue
getQueue prio = gets (\ks -> ksReadyQueues ks ! (prio))

setQueue :: Priority -> ReadyQueue -> Kernel ()
setQueue prio q = modify (\ks -> ks { ksReadyQueues = (ksReadyQueues ks)//[((prio),q)] })

getSchedulerAction :: Kernel SchedulerAction
getSchedulerAction = gets ksSchedulerAction

setSchedulerAction :: SchedulerAction -> Kernel ()
setSchedulerAction a = modify (\ks -> ks { ksSchedulerAction = a })

loopyThread :: String -> ThreadContext
loopyThread id = do
    lift $ putStrLn ("Init thread: " ++ id)
    whileM (return True) (do
        lift $ putStrLn ("In thread: " ++ id)
        yield ThreadPreempted)
    lift $ putStrLn ("End of thread: " ++ id)
    return ("Return: " ++ id)

idle_thread = TCB
    IdleThreadState 0 False 5 Nothing (loopyThread "[idle]") 0

root_thread = TCB
    Running 255 False 5 Nothing (loopyThread "[root]") 1

init_kernel_state = KernelState
    (array (0,255) [(i, []) | i <- [0..255]])
    root_thread
    idle_thread
    ResumeCurrentThread
    0

getThreadState :: TCB -> Kernel ThreadState
getThreadState tcb = return (tcbState tcb)

isRunnable :: TCB -> Kernel Bool
isRunnable thread = do
        state <- getThreadState thread
        return $ case state of
            Running -> True
            Restart -> True
            _ -> False

tcbSchedEnqueue :: TCB -> Kernel ()
tcbSchedEnqueue thread = do
    let queued = tcbQueued thread
    unless queued $ do
        let prio = tcbPriority thread
        queue <- getQueue prio
        setQueue prio $ thread{tcbQueued=True} : queue

tcbSchedDequeue :: TCB -> Kernel ()
tcbSchedDequeue thread = do
    let queued = tcbQueued thread
    when queued $ do
        let prio = tcbPriority thread
        queue <- getQueue prio
        let queue' = filter (/=thread) queue
        setQueue prio queue'

tcbSchedAppend :: TCB -> Kernel ()
tcbSchedAppend thread = do
    let queued = tcbQueued thread
    unless queued $ do
        let prio = tcbPriority thread
        queue <- getQueue prio
        setQueue prio $ queue ++ [thread{tcbQueued=True}]

scheduleSwitchThreadFastfail :: TCB -> TCB -> Priority -> Priority -> Kernel (Bool)
scheduleSwitchThreadFastfail curThread idleThread curPrio targetPrio =
    if curThread /= idleThread
    then return (targetPrio < curPrio)
    else return True

getHighestPrio :: Kernel (Priority)
getHighestPrio = do
    readyQueues <- gets ksReadyQueues
    let lengths = fmap length (elems readyQueues)
    let highest_prio = 255 - (length $ (takeWhile (==0)) . reverse $ lengths)
    return $ fromIntegral $ highest_prio


isHighestPrio :: Priority -> Kernel (Bool)
isHighestPrio p = do
    hprio <- getHighestPrio
    return (p >= hprio)

runnableThreadsExist :: Kernel Bool
runnableThreadsExist = do
    readyQueues <- gets ksReadyQueues
    let lengths = fmap length (elems readyQueues)
    let highest_prio = length $ (takeWhile (==0)) $ lengths
    return $ highest_prio /= length (elems readyQueues)


schedule :: Kernel ()
schedule = do
        curThread <- getCurThread
        action <- getSchedulerAction
        case action of
             ResumeCurrentThread -> return ()
             SwitchToThread candidate -> do
                 wasRunnable <- isRunnable curThread
                 when wasRunnable (tcbSchedEnqueue curThread)

                 idleThread <- getIdleThread
                 let targetPrio = tcbPriority candidate
                 let curPrio = tcbPriority curThread
                 fastfail <- scheduleSwitchThreadFastfail curThread idleThread curPrio targetPrio

                 highest <- isHighestPrio targetPrio

                 if (fastfail && not highest)
                     then do
                         tcbSchedEnqueue candidate
                         setSchedulerAction ChooseNewThread
                         scheduleChooseNewThread
                     else if wasRunnable && curPrio == targetPrio
                             then do
                                 tcbSchedAppend candidate
                                 setSchedulerAction ChooseNewThread
                                 scheduleChooseNewThread
                             else do
                                 switchToThread candidate
                                 setSchedulerAction ResumeCurrentThread
             ChooseNewThread -> do
                 curRunnable <- isRunnable curThread
                 when curRunnable $ tcbSchedEnqueue curThread
                 scheduleChooseNewThread


chooseThread :: Kernel ()
chooseThread = do
    existRunnables <- runnableThreadsExist
    if existRunnables
        then do
            prio <- getHighestPrio
            queue <- getQueue prio
            let thread = head queue
            runnable <- isRunnable thread
            assert runnable
                switchToThread thread
        else
            switchToIdleThread

scheduleChooseNewThread :: Kernel ()
scheduleChooseNewThread = do
    chooseThread
    setSchedulerAction ResumeCurrentThread

switchToThread :: TCB -> Kernel ()
switchToThread thread = do
        tcbSchedDequeue thread
        setCurThread $ thread{tcbQueued=False}

switchToIdleThread :: Kernel ()
switchToIdleThread = do
        thread <- getIdleThread
        setCurThread thread


timeSlice :: Int
timeSlice = 2

rescheduleRequired :: Kernel ()
rescheduleRequired = do
    action <- getSchedulerAction
    case action of
        SwitchToThread target -> do
            tcbSchedEnqueue target
        _ -> return ()
    setSchedulerAction ChooseNewThread

timerTick :: Kernel ()
timerTick = do
  thread <- getCurThread
  state <- getThreadState thread
  modify (\ks -> ks { ksTimeStamp = ksTimeStamp ks + 1 })
  case state of
    Running -> do
      let ts = tcbTimeSlice thread
      let ts' = ts - 1
      if (ts' > 0)
        then setCurThread $ thread{ tcbTimeSlice = ts' }
        else do
           setCurThread $ thread{ tcbTimeSlice = timeSlice }
           tcbSchedAppend thread
           rescheduleRequired
    _ -> return ()


kernelTick :: Kernel()
kernelTick = do
    timerTick
    schedule

-- Kernel picks this to stop running
dieThread :: ThreadContext
dieThread = do
    lift $ putStrLn ("Dying...")
    return ("Returning dead")

kernelDo :: Show a => KernelState -> Yield a ThreadContext -> (KernelState, ThreadContext)
kernelDo ks (Yield x newThreadContext) =
    (ks', lift (putStrLn (show x ++ " @T=" ++ show (ksTimeStamp ks))) >> pickThread)
        where ks' = snd $ runState kernelTick (ks_updated_context)

              ks_updated_context :: KernelState
              ks_updated_context =
                  if ksTimeStamp ks > 0 then
                      ks{ksCurThread = theTCB{tcbContext=newThreadContext}}
                  else
                      ks
                  where theTCB = ksCurThread ks

              pickThread :: ThreadContext
              pickThread = if (ksTimeStamp ks' < 10)
                              then tcbContext $ ksCurThread ks'
                              else dieThread


runKernel :: ThreadContext -> IO (KernelState, String)
runKernel = foldRun kernelDo init_kernel_state

main :: IO ()
main = do
    (resultState, resultString) <- runKernel (loopyThread "INIT")
    putStrLn ("Kernel returned: " ++ resultString)
    putStrLn ("timeStamp: " ++ (show $ ksTimeStamp resultState))
