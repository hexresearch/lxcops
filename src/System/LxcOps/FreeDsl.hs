module System.LxcOps.FreeDsl where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Free
import Data.Bool
import Data.Word
import System.Exit

import System.LxcOps.Types  -- FIXME reexport types

data LxcOpF a
  = Info String (Maybe ContainerInfo -> a)
  | IsDefined String (Bool -> a)
  | Create CreateParams (Bool -> a)
  | Start
    { startName :: String
    , startUseLxcInit :: Bool
    , startArgs :: [String]
    , startVA :: (Bool -> a)
    }
  | Wait String [ContainerState] (Maybe Int) (Bool -> a)
  | Stop String (Bool -> a)
  | Destroy String (Bool -> a)
  | Clone CloneParams (Maybe Container -> a)
  | AttachRunWait
    { attachName    :: String                -- ^ Container name
    , attachOptions :: AttachOptions         -- ^ Attach options.
    , attachPath    :: String                -- ^ Full path inside container of program to run.
    , attachArgs    :: [String]              -- ^ Array of arguments to pass to program.
    , attachVA      :: (Maybe ExitCode -> a) -- ^ @waitpid(2)@ status of exited process that ran program, or @Nothing@ on error.
    }
  | GetInterfaces String ([String] -> a)
  | GetIPs
    { ipsName      :: String        -- ^ ContainerName
    , ipsIfaceName :: String        -- ^ Network interface name to consider.
    , ipsFamily    :: String        -- ^ Network family (for example @"inet"@, @"inet6"@).
    , ipsIPv6Scope :: Word32        -- ^ IPv6 scope id (ignored if family is not "inet6").
    , ipsVA        :: ([String] -> a) -- ^ A list of network interfaces.
    }
  deriving (Functor)

isDefinedF :: (Monad m) => String -> FreeT LxcOpF m Bool
isDefinedF name = liftF $ IsDefined name id

containerInfoF :: (Monad m) => String -> FreeT LxcOpF m (Maybe ContainerInfo)
containerInfoF name = liftF $ Info name id

startF :: (Monad m) => String -> Bool -> [String] -> FreeT LxcOpF m Bool
startF name useinit argv = liftF $ Start name useinit argv id

waitF :: (Monad m) => String -> [ContainerState] -> Maybe Int -> FreeT LxcOpF m Bool
waitF name ss mt = liftF $ Wait name ss mt id

stopF :: (Monad m) => String -> FreeT LxcOpF m Bool
stopF name = liftF $ Stop name id

destroyF :: (Monad m) => String -> FreeT LxcOpF m Bool
destroyF name = liftF $ Destroy name id

-- | Run a program inside a container and wait for it to exit.
attachRunWaitF
  :: (Monad m)
  => String               -- ^ Container name
  -> AttachOptions        -- ^ Attach options.
  -> String               -- ^ Full path inside container of program to run.
  -> [String]             -- ^ Array of arguments to pass to program.
  -> FreeT LxcOpF m (Maybe ExitCode) -- ^ @waitpid(2)@ status of exited process that ran program, or @Nothing@ on error.
attachRunWaitF name opts prg argv = liftF $ AttachRunWait name opts prg argv id

getInterfacesF :: (Monad m) => String -> FreeT LxcOpF m [String]
getInterfacesF name = liftF $ GetInterfaces name id

-- | Determine the list of container IP addresses.
getIPsF
  :: (Monad m)
  => String        -- ^ Container name
  -> String        -- ^ Network interface name to consider.
  -> String        -- ^ Network family (for example @"inet"@, @"inet6"@).
  -> Word32        -- ^ IPv6 scope id (ignored if family is not "inet6").
  -> FreeT LxcOpF m [String] -- ^ A list of network interfaces.
getIPsF name iface fam sid = liftF $ GetIPs name iface fam sid id
