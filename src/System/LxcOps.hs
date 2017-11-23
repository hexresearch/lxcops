module System.LxcOps where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Free
import Data.Bool
import Data.Word
import System.Exit

import System.LxcOps.Types  -- FIXME reexport types
import System.LxcOps.Wrapper
import System.LxcOps.FreeDsl

type LxcOpIO = FreeT LxcOpF IO

-- | Lxc monad that holds one container
newtype LXC a = LxcM { unLxcM :: ReaderT Container LxcOpIO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Container)

-- | Lift LxcIO monad to LXC monad
liftLxcOpIO :: LxcOpIO a -> LXC a
liftLxcOpIO = LxcM . lift

-- | Execution of 'LXC'
runLxcM :: Container -> LXC a -> LxcOpIO a
runLxcM e = flip runReaderT e . unLxcM

-- | Execution of 'LXC' in IO monad
runLxcMIO :: Container -> LXC a -> IO a
runLxcMIO c m = runLxcOpTIO $ runLxcM c m

runLxcOpTIO :: LxcOpIO a -> IO a
runLxcOpTIO t = runFreeT t >>= \case
  Pure r -> return r
  Free (IsDefined n next) -> lxcIsDefined n >>= runLxcOpTIO . next
  Free (Info n next) -> lxcInfo n >>= runLxcOpTIO . next
  Free (Create cps next) -> lxcCreate cps >>= runLxcOpTIO . next
  Free (Start n useinit argv next) ->
    lxcStart n useinit argv >>= runLxcOpTIO . next
  Free (Wait n ss mt next) -> lxcWait n ss mt >>= runLxcOpTIO . next
  Free (Stop n next) -> lxcStop n >>= runLxcOpTIO . next
  Free (Destroy n next) -> lxcDestroy n >>= runLxcOpTIO . next
  Free (AttachRunWait n opts prg argv next) ->
    lxcAttachRunWait n opts prg argv >>= runLxcOpTIO . next
  Free (GetInterfaces n next) -> lxcGetInterfaces n >>= runLxcOpTIO . next
  Free (GetIPs n iface fam sid next) ->
    lxcGetIPs n iface fam sid >>= runLxcOpTIO . next
  Free (Clone cps next) -> lxcClone cps >>= runLxcOpTIO . next

-- | Run @'LXC' a@ computation for a given 'Container'.
--
withContainer :: MonadIO m => Container -> LXC a -> m a
withContainer c m = liftIO $ runLxcMIO c m

-- | Create a container.
create :: String            -- ^ Template to execute to instantiate the root filesystem and adjust the configuration.
       -> Maybe String      -- ^ Backing store type to use (if @Nothing@, @dir@ type will be used by default).
       -> Maybe BDevSpecs   -- ^ Additional parameters for the backing store (for example LVM volume group to use).
       -> [CreateOption]    -- ^ 'CreateOption' flags. /Note: LXC 1.0 supports only @CreateQuiet@ option./
       -> [String]          -- ^ Arguments to pass to the template.
       -> LXC Bool          -- ^ @True@ on success. @False@ otherwise.
create t bdevtype bdevspecs flags argv = do
    name <- asks containerName
    let cps = CreateParams name t bdevtype bdevspecs flags argv
    liftLxcOpIO . liftF $ Create cps id

-- | Start the container.
start :: Bool       -- ^ Use @lxcinit@ rather than @\/sbin\/init@.
      -> [String]   -- ^ Array of arguments to pass to init.
      -> LXC Bool   -- ^ @True@ on success, else @False@.
start useinit argv = do
    name <- asks containerName
    liftLxcOpIO $ startF name useinit argv

isDefined :: LXC Bool
isDefined = do
    name <- asks containerName
    liftLxcOpIO $ isDefinedF name

containerInfo :: LXC (Maybe ContainerInfo)
containerInfo = asks containerName >>= liftLxcOpIO . containerInfoF

-- | Wait for container to reach a particular state.
--
-- * A timeout of @-1@ means wait forever.
-- A timeout @0@ means do not wait.
wait :: ContainerState  -- ^ State to wait for.
     -> Int             -- ^ Timeout in seconds.
     -> LXC Bool        -- ^ @True@ if state reached within timeout, else @False@.
wait s t = wait' [s] $ bool Nothing (Just t) (t >= 0)

-- | Wait for container to reach a particular states.
--
-- * A timeout of Nothing means wait forever.
-- A timeout @0@ means do not wait.
wait' :: [ContainerState]  -- ^ State to wait for.
     -> Maybe Int          -- ^ Timeout in seconds.
     -> LXC Bool           -- ^ @True@ if state reached within timeout, else @False@.
wait' ss mt = do
    name <- asks containerName
    liftLxcOpIO $ waitF name ss mt

-- @True@ on success, else @False@.
stop :: LXC Bool
stop = asks containerName >>= liftLxcOpIO . stopF

-- * NOTE: Container must be stopped and have no dependent snapshots.
destroy :: LXC Bool
destroy = asks containerName >>= liftLxcOpIO . destroyF

-- | Create a container snapshot.
--
-- Assuming default paths, snapshots will be created as
-- @\/var\/lib\/lxc\/\<c\>\/snaps\/snap\<n\>@
-- where @\<c\>@ represents the container name and @\<n\>@
-- represents the zero-based snapshot number.
snapshot :: Maybe FilePath  -- ^ Full path to file containing a description of the snapshot.
         -> LXC (Maybe Int) -- ^ @Nothing@ on error, or zero-based snapshot number.
snapshot path = pure Nothing  -- Unused for `dir` bdevtype

-- | Run a program inside a container and wait for it to exit.
attachRunWait :: AttachOptions        -- ^ Attach options.
              -> String               -- ^ Full path inside container of program to run.
              -> [String]             -- ^ Array of arguments to pass to program.
              -> LXC (Maybe ExitCode) -- ^ @waitpid(2)@ status of exited process that ran program, or @Nothing@ on error.
attachRunWait opts prg argv = do
    name <- asks containerName
    liftLxcOpIO $ attachRunWaitF name opts prg argv

-- | Determine the list of container IP addresses.
getIPs :: String        -- ^ Network interface name to consider.
       -> String        -- ^ Network family (for example @"inet"@, @"inet6"@).
       -> Word32        -- ^ IPv6 scope id (ignored if family is not "inet6").
       -> LXC [String]  -- ^ A list of network interfaces.
getIPs iface fam sid = do
    name <- asks containerName
    liftLxcOpIO $ getIPsF name iface fam sid

-- | Obtain a list of network interfaces.
getInterfaces :: LXC [String]
getInterfaces = asks containerName >>= liftLxcOpIO . getInterfacesF

-- | Copy a stopped container.
clone :: Maybe String   -- ^ New name for the container. If @Nothing@, the same name is used and a new lxcpath MUST be specified.
      -> Maybe FilePath -- ^ lxcpath in which to create the new container. If @Nothing@, the original container's lxcpath will be used.
      -> [CloneOption]  -- ^ Additional 'CloneOption' flags to change the cloning behaviour.
      -> Maybe String   -- ^ Optionally force the cloned bdevtype to a specified plugin. By default the original is used (subject to snapshot requirements).
      -> Maybe String   -- ^ Information about how to create the new storage (i.e. fstype and fsdata).
      -> Maybe Word64   -- ^ In case of a block device backing store, an optional size. If @Nothing@, the original backing store's size will be used if possible. Note this only applies to the rootfs. For any other filesystems, the original size will be duplicated.
      -> [String]       -- ^ Additional arguments to pass to the clone hook script.
      -> LXC (Maybe Container)  -- ^ Newly-allocated copy of container @c@, or @Nothing@ on error.
clone newname lxcpath flags bdevtype bdevdata newsize hookargs = do
-- clone (Just name) Nothing [CloneSnapshot] Nothing Nothing Nothing []
    name <- asks containerName
    let cps = CloneParams name newname lxcpath flags bdevtype bdevdata newsize hookargs
    liftLxcOpIO . liftF $ Clone cps id
