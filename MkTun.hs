import Control.Applicative
import qualified Network.TunTap.Omni as T
import Network.BSD
import Network.Socket
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import Control.Concurrent.Async
import System.IO
import System.Environment
import Control.Monad

pause tag = do
  putStrLn tag
  getLine
  return ()

resolveHost :: String -> IO HostAddress
resolveHost name = do
  addr:_ <- hostAddresses <$> getHostByName name
  return addr

main = do
  args <- getArgs
  case args of
    [action, portS, vHost, vDst] -> do
      hostAddr <- resolveHost vHost
      mask <- resolveHost "255.255.255.0"
      dstAddr <- resolveHost vDst

      dev <- T.new Nothing T.TUN True
      T.bringUp dev
      T.setIpMaskDst hostAddr mask dstAddr dev
      mtu <- T.getMtu dev

      putStrLn $ "created: " ++ show dev ++ ", mtu = " ++ show mtu
      hDev <- T.toHandle dev
      runTun action (read portS) hDev mtu

    _ -> do
      putStrLn "Usage: <program> serve|connect port addr dstAddr"

runTun :: String -> Int -> Handle -> Int -> IO ()
runTun "serve" port hDev mtu = do
  s <- mkTcpSock
  bindSocket s $ SockAddrInet (fromIntegral port) 0
  listen s 5
  (peer, _) <- accept s
  sClose s
  hPeer <- socketToHandle peer ReadWriteMode
  hSetBuffering hPeer NoBuffering
  runLoop hPeer hDev mtu

runTun "connect" port hDev mtu = do
  remote <- resolveHost "127.0.0.1"
  peer <- mkTcpSock
  connect peer $ SockAddrInet (fromIntegral port) remote
  hPeer <- socketToHandle peer ReadWriteMode
  hSetBuffering hPeer NoBuffering
  runLoop hPeer hDev mtu

runLoop :: Handle -> Handle -> Int -> IO ()
runLoop hPeer hDev mtu = do
  putStrLn "[runLoop] started"
  t1 <- async $ forever runPeerToDev
  t2 <- async $ forever runDevToPeer
  void $ waitAnyCancel [t1, t2]
 where
  runPeerToDev = do
    sizeBs <- B.hGet hPeer 4
    let Right size = fromIntegral <$> S.runGet S.getWord32be sizeBs
    --putStrLn $ "[runPeerToDev] " ++ show size ++ " bytes"
    pkt <- B.hGet hPeer size
    B.hPut hDev pkt

  runDevToPeer = do
    pkt <- B.hGetSome hDev mtu
    let pktLen = B.length pkt
    --putStrLn $ "[runDevToPeer] " ++ show pktLen ++ " bytes"
    let sizeBs = S.runPut $ S.putWord32be $ fromIntegral pktLen
    B.hPut hPeer sizeBs
    B.hPut hPeer pkt 


mkTcpSock = do
  s <- socket AF_INET Stream 0
  setSocketOption s ReuseAddr 1
  return s

