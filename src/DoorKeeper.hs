-- file: DoorKeeper

module DoorKeeper where

import Homer
import KayleDefined
import KayleConst
import Modules.ConfigReader

import Network.Socket

doorKeeper :: KayleEnv -> IO ()
doorKeeper env = do
  let cfgs = envCfg env
      room = envRoom env

  masterSock <- listenSockGet cfgs
  doorKeeperWork env masterSock

  where
    doorKeeperWork :: KayleEnv -> Socket -> IO ()
    doorKeeperWork env sock = do
      homer <- waitHomer sock
      return ()

listenSockGet :: Configs -> IO Socket
listenSockGet cfgs = do
  let serverOpts = configGet cfgs serverInfoGet serverAddr_err_msg

  (serverAddr:xs) <- getAddrInfo Nothing (Just $ addr serverOpts) (Just $ port serverOpts)

  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 10

  return sock
