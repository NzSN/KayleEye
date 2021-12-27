-- KayleHome Collect judgements of kayle and then execute if these judgements is right.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace ()

import Data.String.Conversions (cs)

-- Http Request
import Network.HTTP.Client ( defaultManagerSettings, newManager )
import Network.HTTP.Types.Status (statusCode)

-- Process, File, Director

import System.Environment ( getArgs )

-- Configuration
import Modules.ConfigReader as C
    ( configGet,
      databaseGet,
      testPiecesGet,
      Configs(Configs_Str, Configs_M, configMap),
      DatabaseInfo_cfg(db, db_pass, db_user, db_host),
      TestProject_cfg(testContent) )

-- Monad Transformers
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

-- Socket
import Network.Socket

-- Map
import Data.Map as Map
    ( filter, foldl, foldlWithKey, insert, lookup, map )
import Data.Map.Merge.Strict ()

import Data.Bool ( bool )

-- List
import Data.List as List
import Control.Monad (void, when)

import Control.Monad.Writer
    ( MonadIO(liftIO), MonadTrans(lift), forever )

-- Maybe
import Data.Maybe ( fromJust, fromMaybe, isNothing )

-- Json
import Data.Aeson ( encode )

-- Database
import Database.HDBC ( catchSql )


import System.Systemd.Daemon (notifyWatchdog)

-- Homer
import Letter as L
    ( allKeysOfContent,
      answerAccepted_Letter,
      answerRejected_Letter,
      isEmptyLetter,
      isTestFinished,
      isTestSuccess,
      letterUpdate',
      retriFromContent,
      retriFromHeader,
      sizeOfLetter,
      str2Ident,
      typeOfLetter,
      typeOfLetter',
      Identity(ident_event, ident_name),
      Letter(Empty_letter, content, ident) )
import Homer as H
import LetterBox
    ( boxInit,
      boxKeyCreate,
      commitKey,
      historyTbl,
      insertLetter,
      isLetterExists,
      procTbl,
      removeLetter,
      searchLetter,
      updateLetter,
      BoxKey )
import KayleConst
    ( k_ok,
      boxKeyRetryInterval,
      content_who,
      db_err_msg,
      isControlEvent,
      k_error,
      minute_micro,
      second_micro_int)
import Logger ( appendLogger, doLogger, LoggerT(runLoggerT) )
import KayleBasics as K ( letterInit, loadConfig )
import RepoOps as R ()
import Actions ( actionSelector, actionSelector' )
import KayleDefined as Def
    ( KayleEnv(KayleEnv, envPuller, envArgs, envRoom, envHomer, envCfg,
               envKey, envRegTbl),
      addItem',
      finished_status,
      getItems,
      identWithSubTest,
      isItemExists,
      isUnRegister,
      kayleEnvSetBKey,
      markFinished,
      newRegisterTbl,
      register,
      removeItems,
      showRegTable,
      unRegister_status,
      unregister,
      RegisterBlock(..),
      RegisterItem(RegItem, tod, regStatus),
      RegisterItems(..),
      RegisterTbl(regTbl) )
import Puller
    ( isLocked, isUnlocked, lock, newPuller, pullerSpawn, unlock )
import Room
import Notifier
import DoorKeeper
import Time

import Control.Concurrent
import Control.Concurrent.STM
import System.IO

type Kayle = ReaderT KayleEnv (LoggerT IO) Integer

runKayle :: Kayle -> KayleEnv -> IO ()
runKayle k e =
  void $ runLoggerT (runReaderT k e)

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file loaded
  args <- getArgs
  configs <- loadConfig (Prelude.head args) (head . tail $ args)

  -- Get Encrypt key from Standard input
  -- and add to configs so cipher in configs
  -- can be decrypt
  encryptKey <- getLine
  let cfgMap = configMap configs
      configs' = Configs_M $ Map.insert "EncryptKey" (Configs_Str encryptKey) cfgMap

  -- Box initialization
  let dbOpts = configGet configs' databaseGet db_err_msg
  bKey <- boxKeyCreate (C.db_host dbOpts) (C.db_user dbOpts) (C.db_pass dbOpts) (C.db dbOpts)

  -- Database init, Create procTbl and historyTbl if not exists
  boxInit bKey

  -- Build the room which supply a way to Kayle to communicate with doorkeeper
  room <- newRoom

  -- Create Notifier and Puller
  notifier <- newNotifier configs'
  puller <- newPuller manager configs' notifier

  -- Create register table
  regTable <- newRegisterTbl

  let env = KayleEnv configs' manager args Empty_Homer bKey room puller regTable notifier

  -- Spawn Puller process
  forkIO $ pullerSpawn puller
  -- Spawn Notifier process
  forkIO $ notifierSpawn notifier
  -- Spawn Doorkeeper process
  forkIO $ doorKeeper env
  -- Spawn Register Maintainer process
  forkIO $ regMaintainer env
  -- Systemd Notify
  forkIO systemdNotify

  runKayle doKayle env


systemdNotify :: IO ()
systemdNotify = forever $ do
  threadDelay $ second_micro_int * 3
  notifyWatchdog


-- Register Table Maintainer
-- fixme: while filtering oudated items should also do
--        some cleaning works related to event mode
regMaintainer :: KayleEnv -> IO ()
regMaintainer env = forever $ do
  -- Current UTC time
  current <- getTimeNow

  -- Sleep in a query interval
  threadDelay queryInterval

  showRegTable tbl

  -- Maintaining the register table
  catchSql (maintain tbl bKey current)
    -- Sql exception is happened
    -- and now jsut deal with 2003 error
    (\e -> print ("Maintainer:" ++ show e)
           -- Renew box key and use it to update
           -- environment then next trun
           >> waitKeyUntil (envCfg env)
           >>= regMaintainer . kayleEnvSetBKey env)


  where tbl = envRegTbl env
        bKey = envKey env

        queryInterval = minute_micro

        -- Function to maintain register table
        maintain :: RegisterTbl -> BoxKey -> TimeOfDay' -> IO ()
        maintain rTbl bKey current = do
          outdatedList <- atomically $ regTblClear rTbl current

          -- Remove outdated item from letterBox
          if Prelude.null outdatedList
            then return ()
            else mapM_ (removeLetter bKey procTbl) outdatedList
                 >> commitKey bKey

        regTblClear :: RegisterTbl -> TimeOfDay' -> STM [String]
        regTblClear tbl' current = do
          tbl_ <- readTVar $ regTbl tbl'
          let itemsOutDated = Map.foldl (eventFold current) [] tbl_
              -- Fixme: Use mapWithKey here to do some extra operation while
              -- clean an outdated test, such as unlock puller while
              -- clean an outdated daily test.
              cleanMap = Map.map (filterOutDated current) tbl_

          -- Update register table
          writeTVar (regTbl tbl') cleanMap

          -- Return items which should be remove from letter box
          return itemsOutDated

        eventFold :: TimeOfDay' -> [String] -> RegisterBlock -> [String]
        eventFold current acc x = acc ++ Map.foldlWithKey (blockFold current) [] (regBlk x)

        blockFold :: TimeOfDay' -> [String] -> String -> RegisterItems -> [String]
        blockFold current acc k x = if isNeedClean current x
                                    then k : acc
                                    else acc

        -- Function to check is a items need clean
        isNeedClean :: TimeOfDay' -> RegisterItems -> Bool
        isNeedClean current items =
          let itemArray = regItems items
          in List.foldl (\acc x -> acc || outDatedCond x current) False itemArray

        outDatedCond :: RegisterItem -> TimeOfDay' -> Bool
        outDatedCond item current =
          regStatus item == unRegister_status &&
          (current Time.- Def.tod item > 1200)

        filterOutDated :: TimeOfDay' -> RegisterBlock -> RegisterBlock
        filterOutDated current m = RegBlk $ Map.filter (not . isNeedClean current) (regBlk m)


-- Append log message to Kayle
logKayle h = lift . appendLogger h

waitKeyUntil :: Configs -> IO BoxKey
waitKeyUntil c =
  let dbOpts = configGet c databaseGet db_err_msg
  in catchSql (boxKeyCreate (C.db_host dbOpts) (C.db_user dbOpts) (C.db_pass dbOpts) (C.db dbOpts))
     (\e -> print (show e)
            >> threadDelay boxKeyRetryInterval
            >> waitKeyUntil c)

doKayle :: Kayle
doKayle =
  ask >>= \l -> do_forever Empty_letter l
  where
    do_forever :: Letter -> KayleEnv -> Kayle
    do_forever l env = do
      let homer = envHomer env
          bKey = envKey env
          room = envRoom env

      liftIO . print $ "Wait letter"

      letter <- if isEmptyLetter l
                then liftIO . getLetter $ room
                else return l

      liftIO . print $ "Received Letter : " ++ show letter

      -- Deal with control letter
      if isControlEvent $ typeOfLetter letter
        then (liftIO . controlProc letter $ env)
             >> (liftIO . showRegTable $ envRegTbl env)
             >> do_forever Empty_letter env

        -- fixme: should provide function to deal with different error type
        else (liftIO . catchSql (procLetter' letter bKey env) $ procHandler)
             >>= \eType ->
                   case eType of
                     -- OK
                     0 -> (liftIO . print $ "OK") >> do_forever Empty_letter env
                     -- ERROR, just retry the letter with new box key.
                     1 -> (liftIO . print $ "ERROR") >> (liftIO . waitKeyUntil $ envCfg env)
                          >>= \x -> let env_new = kayleEnvSetBKey env x
                                    in do_forever letter env_new

    procHandler = \e -> print e >>  return k_error :: IO Integer
    procLetter' l b e =
      doLogger (runReaderT (procLetter l b e) e) (last $ envArgs e)
      >>= \retCode -> commitKey b >> return retCode

    -- Function to process new incomming letter
    procLetter :: Letter -> BoxKey -> KayleEnv -> Kayle
    procLetter letter bKey env = do
      liftIO . print $ "procLetter"
      logKayle "Info" $ "Received Letter : " ++ show letter
      exists <- liftIO . isLetterExists bKey historyTbl $ ident letter
      if not exists
        then (liftIO . isLetterExists bKey procTbl $ ident letter)
             >>= \exists -> if not exists
                            then newLetter letter env
                            else inProcLetter letter env
        else action' True letter $ env

    -- Function to deal with the first arrived letter of a project
    newLetter :: Letter -> KayleEnv -> Kayle
    newLetter l env =
        let cfgs = envCfg env
            bKey = envKey env
        in (\x -> if isNothing x
                then logKayle "Error" "letterInit failed"
                else newLetterProc l (fromJust x) env bKey) (letterInit cfgs l)

    -- Function to deal with situation there is only one test content in config
    newLetterProc :: Letter -- Letter generate by letterInit
                    -> Letter -- Letter is received from KayleEye
                    -> KayleEnv
                    -> BoxKey
                    -> Kayle
    newLetterProc rl l e b = if sizeOfLetter l == 1
                        then newLetterInsert l historyTbl b >>
                                if isTestSuccess l
                                then action True rl e
                                else action False rl e
                        else newLetterInsert l procTbl b

    -- Function to insert new letter into table
    newLetterInsert :: Letter -> String -> BoxKey -> Kayle
    newLetterInsert l tbl k =
        logKayle "Info" ("Insert letter : " ++ show l ++ " Into " ++ tbl)
        >> (liftIO . insertLetter k tbl) l
        >> return k_ok

    -- Function to process inProc letter
    inProcLetter :: Letter -> KayleEnv -> Kayle
    inProcLetter l env =
        let cfgs = envCfg env
            bKey = envKey env
        in (liftIO . runMaybeT $ searchLetter bKey procTbl (ident l))
            >>= (\x -> if isNothing x
                    then logKayle "Warning" "Letter doesn't exists"
                    else inProcDo l (fromJust x) env)

    -- Function to update received letter into box
    inProcDo :: Letter -- The recevied letter
                -> Letter -- The letter from box
                -> KayleEnv -> Kayle
    inProcDo rl bl env =
        let content = L.content rl
            lUpdated = letterUpdate' bl [(k, fromJust $ Map.lookup k content) | k <- allKeysOfContent rl]
            -- Update the letter from box
        in (return $ lUpdated)
            -- Put the letter from box back to box
            >>= (\x -> logKayle "Info" ("Update letter : " ++ show x) >>
                    (liftIO . updateLetter (envKey env) (L.ident x) (encode $ L.content x) $ isTestFinished x))
            -- To check that whether the test describe by the letter is done
            >>= (\x -> if x == 1
                         then if isTestSuccess lUpdated
                              then logKayle "Info" "Accpet Letter" >> action True rl env
                              else logKayle "Info" "Denie Letter" >> action False rl env
                         else return k_ok)

controlProc :: Letter -> KayleEnv -> IO ()
controlProc l env =
  let proc = case typeOfLetter l of
               "register"   -> return registerProc
               "unRegister" -> return unRegisterProc
               "terminated" -> return terminatedProc
  in maybe (return ()) (\proc -> proc l env) proc

registerProc :: Letter -> KayleEnv -> IO ()
registerProc l env = do
  let subTest = retriFromContent l content_who

  if isNothing testArray
    then return ()
    else maybe (return ()) (doRegister e i) subTest

  where room = envRoom env
        regTable = envRegTbl env

        -- Get the test set which the letter belong to
        testSet = testPiecesGet (ident_name identity) (envCfg env)
        testArray = Map.lookup (ident_name identity) $ testContent $ fromJust testSet

        identity = str2Ident (ident l)
        e = ident_event identity
        i = ident l

        mapFunc tod sub = addItem' regTable e i $ RegItem sub unRegister_status tod

        rejectedAck = answerRejected_Letter (identWithSubTest l)
        acceptedAck = answerAccepted_Letter (identWithSubTest l)

        -- Register processing function
        doRegister e i sub = do
          isExists <- isItemExists regTable e i sub

          if isExists
            then isUnRegister regTable e i sub
                 >>= bool
                 (putLetter room rejectedAck)
                 (Def.register regTable e i sub
                  >> putLetter room acceptedAck)
            else getTimeNow
                 >>= (\tod -> Prelude.mapM_ (mapFunc tod) (fromJust testArray))
                 >> Def.register regTable e i sub
                 >> putLetter room acceptedAck
                 >> postRegister l env

unRegisterProc :: Letter -> KayleEnv -> IO ()
unRegisterProc l env = do
  let identity = str2Ident (ident l)
      e = ident_event identity
      i = ident l
      subTest = retriFromContent l content_who

  maybe (return ()) (doUnRegister e i) subTest

  where room = envRoom env
        regTable = envRegTbl env

        doUnRegister e i sub = do
          isExists <- isItemExists regTable e i sub

          when isExists $ Def.unregister regTable e i sub

terminatedProc :: Letter -> KayleEnv -> IO ()
terminatedProc l env =
  let sub = fromMaybe "" subMay
  in markFinished rTbl e i sub
     >> getItems rTbl e i
     >>= \items -> let itemArray = fromMaybe (RegItems []) items
                   in when (isItemsDone itemArray) $ removeItems rTbl e i >> postTerminated l env
  where
        rTbl = envRegTbl env
        e = ident_event identity
        i = ident l
        subMay = retriFromContent l content_who
        identity = str2Ident (ident l)

        isItemsDone items =
          let itemArray = regItems items
          in List.foldl (\acc x -> acc && (regStatus x == finished_status)) True itemArray

postRegister :: Letter -> KayleEnv -> IO ()
postRegister l env =
  let proc = case typeOfLetter' l of
               "merge_request" -> return mergePostRegister
               "push"          -> return pushPostRegister
               "daily"         -> return dailyPostRegister
  in maybe (return ()) (\proc -> proc l env) proc

  where mergePostRegister l env = return ()
        pushPostRegister l env = return ()
        dailyPostRegister l env =
          isUnlocked puller
          >>= \x -> when x $ lock puller
        puller = envPuller env

postTerminated :: Letter -> KayleEnv -> IO ()
postTerminated l env =
  let proc = case typeOfLetter' l of
               "merge_request" -> return $ mergePostTerminated
               "push"          -> return $ pushPostTerminated
               "daily"         -> return $ dailyPostTerminated
  in maybe (return ()) (\proc -> proc l env) proc

  where mergePostTerminated l env = return ()
        pushPostTerminated l env = return ()
        dailyPostTerminated l env = do
          let puller = envPuller env
          locked <- isLocked puller
          when locked $ unlock puller

-- Action be perform after test project done
action :: Bool -> Letter -> KayleEnv -> Kayle
action success l env =
  let event = fromJust $ retriFromHeader l "event"
  in liftIO . actionSelector event success l $ env
action' :: Bool -> Letter -> KayleEnv -> Kayle
action' success l env =
  let event = fromJust $ retriFromHeader l "event"
  in liftIO . actionSelector' event success l $ env
