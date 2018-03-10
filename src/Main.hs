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
        deriving (Show, Eq)

data Notification = Notification {
    ntfnObj :: NTFN,
    ntfnBoundTCB :: Maybe TCB }
        deriving (Show, Eq)

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
        | SysSpawnThread Priority ThreadContext (Maybe Notification)
    deriving (Show, Eq)

data ThreadStatus =
    ThreadPreempted | ThreadSyscall Syscall
        deriving (Show, Eq)

type ThreadContext = Coroutine (Yield ThreadStatus) IO String

instance Show ThreadContext where
    show tc = "<ThreadContext>"

instance Eq ThreadContext where
    x == y = False

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

puts = lift . putStrLn
atomic op = do
    op
    yield ThreadPreempted
syscall x = yield $ ThreadSyscall x

loopyThread :: String -> ThreadContext
loopyThread id = do
    atomic $ puts ("Init thread: " ++ id)
    whileM (return True) (do
        atomic $ puts ("In thread: " ++ id)
        )
    atomic $ puts ("End of thread: " ++ id)
    return ("Return: " ++ id)

rootThread :: String -> ThreadContext
rootThread id = do
    atomic $ puts ("Init thread: " ++ id)
    syscall $ SysSpawnThread 255 (loopyThread "[child1]") Nothing
    syscall $ SysSpawnThread 255 (loopyThread "[child2]") Nothing
    whileM (return True) (do
        atomic $ puts ("In thread: " ++ id)
        )
    atomic $ puts ("End of thread: " ++ id)
    return ("Return: " ++ id)

timeSlice :: Int
timeSlice = 5

idle_thread = TCB
    IdleThreadState 0 False timeSlice Nothing (loopyThread "[idle]") 0

root_thread = TCB
    Running 255 False timeSlice Nothing (rootThread "[root]") 1

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
        traceShow action $ (do
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
                 scheduleChooseNewThread
                           )


chooseThread :: Kernel ()
chooseThread = do
    existRunnables <- runnableThreadsExist
    if existRunnables
        then do
            prio <- getHighestPrio
            queue <- getQueue prio
            let thread = head queue
            traceShow queue (do
            runnable <- isRunnable thread
            assert runnable
                switchToThread thread
                            )
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
  ks <- get
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
           thread <- getCurThread
           tcbSchedAppend thread
           rescheduleRequired
    _ -> return ()

activateThread :: Kernel ()
activateThread = do
        thread <- getCurThread
        state <- getThreadState thread
        case state of
            Running -> return ()
            Restart -> do
                modify (\ks -> ks { ksCurThread = thread{tcbState=Running} })
            IdleThreadState -> do
                return ()
            _ -> fail $ "Current thread is blocked, state: " ++ show state


kernelTick :: Kernel()
kernelTick = do
    timerTick
    schedule
    activateThread

-- Kernel picks this to stop running
dieThread :: ThreadContext
dieThread = do
    lift $ putStrLn ("Dying...")
    return ("Returning dead")

kernelDo :: KernelState -> Yield ThreadStatus ThreadContext -> (KernelState, ThreadContext)
kernelDo ks (Yield x newThreadContext) =
    (ks', lift (putStrLn (show x ++ " @T=" ++ show (ksTimeStamp ks))) >> pickThread)
        where ks' = snd $ runState (actOn x) (ks_updated_context)

              actOn :: ThreadStatus -> Kernel ()
              actOn ThreadPreempted = kernelTick
              actOn (ThreadSyscall (SysSpawnThread prio ctxt ntfn)) = do
                  tcbSchedEnqueue $ TCB Restart prio False timeSlice ntfn ctxt (ksTimeStamp ks)
                  kernelTick
              actOn _ = assert False $ return () --unknown thread status / syscall?

              ks_updated_context :: KernelState
              ks_updated_context =
                  if ksTimeStamp ks > 0 then
                      ks{ksCurThread = theTCB{tcbContext=newThreadContext}}
                  else
                      ks
                  where theTCB = ksCurThread ks

              pickThread :: ThreadContext
              pickThread = if (ksTimeStamp ks' < 20)
                              then tcbContext $ ksCurThread ks'
                              else dieThread


runKernel :: ThreadContext -> IO (KernelState, String)
runKernel = foldRun kernelDo init_kernel_state

main :: IO ()
main = do
    (resultState, resultString) <- runKernel (loopyThread "INIT")
    putStrLn ("Kernel returned: " ++ resultString)
    putStrLn ("timeStamp: " ++ (show $ ksTimeStamp resultState))
