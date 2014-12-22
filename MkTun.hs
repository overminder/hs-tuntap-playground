import Control.Applicative
import qualified Network.TunTap.Omni as T
import Network.BSD (getHostByName, HostEntry (hostAddresses))
import qualified Data.ByteString as B

pause tag = do
  putStrLn tag
  getLine
  return ()

main = do
  host:_ <- hostAddresses <$> getHostByName "10.0.0.1"
  mask:_ <- hostAddresses <$> getHostByName "255.255.255.0"

  dev <- T.new Nothing T.TUN True

  putStrLn $ "trying to bring it up..."
  T.bringUp dev

  T.setIpAndMask host mask dev

  mtu <- T.getMtu dev
  hDev <- T.toHandle dev

  putStrLn $ "created: " ++ show dev ++ ", mtu = " ++ show mtu
  pause "press any key to read once."

  bs <- B.hGetSome hDev mtu

  putStrLn $ "Read " ++ show (B.length bs) ++ " bytes."

  pause "ok, press any key to exit."

  return ()
