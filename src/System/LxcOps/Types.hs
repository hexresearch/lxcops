module System.LxcOps.Types where

import Data.IP
import Data.Word
import System.Posix.Types

-- | Container object.
data Container = Container
  { containerName       :: String         -- ^ Container name.
  , containerConfigPath :: Maybe String   -- ^ Container config path.
  }
  deriving (Show)

-- | Container state.
data ContainerState
  = ContainerStopped            -- ^ Container is stopped.
  | ContainerStarting           -- ^ Container is starting.
  | ContainerRunning            -- ^ Container is running.
  | ContainerStopping           -- ^ Container is stopping.
  | ContainerAborting           -- ^ Container is aborting.
  | ContainerFreezing           -- ^ Container is freezing.
  | ContainerFrozen             -- ^ Container is frozen.
  | ContainerThawed             -- ^ Container is thawed.
  | ContainerOtherState String  -- ^ Container is in some other state.
  deriving (Eq, Show)

showState :: ContainerState -> String
showState = \case
  ContainerStopped -> "STOPPED"
  ContainerStarting -> "STARTING"
  ContainerRunning -> "RUNNING"
  ContainerStopping -> "STOPPING"
  ContainerAborting -> "ABORTING"
  ContainerFreezing -> "FREEZING"
  ContainerFrozen -> "FROZEN"
  ContainerThawed -> "THAWED"
  ContainerOtherState s -> s

data ContainerInfo = ContainerInfo
  { ciName  :: String
  , ciState :: ContainerState
  , ciIP    :: Maybe IP
  }
  deriving (Eq, Show)

-- | Options for 'create' operation.
data CreateOption
  = CreateQuiet          -- ^ Redirect @stdin@ to @\/dev\/zero@ and @stdout@ and @stderr@ to @\/dev\/null@.
  | CreateMaxFlags       -- ^ Number of @LXC_CREATE*@ flags.
  deriving (Eq, Ord)

-- | Options for 'clone' operation.
data CloneOption
  = CloneKeepName        -- ^ Do not edit the rootfs to change the hostname.
  | CloneKeepMacAddr     -- ^ Do not change the MAC address on network interfaces.
  | CloneSnapshot        -- ^ Snapshot the original filesystem(s).
  | CloneKeepBDevType    -- ^ Use the same bdev type.
  | CloneMaybeSnapshot   -- ^ Snapshot only if bdev supports it, else copy.
  | CloneMaxFlags        -- ^ Number of @LXC_CLONE_*@ flags.
  deriving (Eq, Ord)

data BDevSpecs = BDevSpecs  -- No use cases now
  deriving (Eq, Ord)

data CreateParams = CreateParams
  { createName      :: String -- ^ Contaier name
  , createTemplate  :: String            -- ^ Template to execute to instantiate the root filesystem and adjust the configuration.
  , createBDevType  :: Maybe String      -- ^ Backing store type to use (if @Nothing@, @dir@ type will be used by default).
  , createBDevSpecs :: Maybe BDevSpecs   -- ^ Additional parameters for the backing store (for example LVM volume group to use).
  , createFlags     :: [CreateOption]    -- ^ 'CreateOption' flags. /Note: LXC 1.0 supports only @CreateQuiet@ option./
  , createArgs      :: [String]          -- ^ Arguments to pass to the template.
  }

data CloneParams = CloneParams
  { cloneOriginalName :: String -- ^ Contaier name
  , cloneMNewName     :: Maybe String   -- ^ New name for the container. If @Nothing@, the same name is used and a new lxcpath MUST be specified.
  , cloneMLxcPath     :: Maybe FilePath -- ^ lxcpath in which to create the new container. If @Nothing@, the original container's lxcpath will be used.
  , cloneOps          :: [CloneOption]  -- ^ Additional 'CloneOption' flags to change the cloning behaviour.
  , cloneMBDevType    :: Maybe String   -- ^ Optionally force the cloned bdevtype to a specified plugin. By default the original is used (subject to snapshot requirements).
  , cloneMInfo        :: Maybe String   -- ^ Information about how to create the new storage (i.e. fstype and fsdata).
  , cloneMSize        :: Maybe Word64   -- ^ In case of a block device backing store, an optional size. If @Nothing@, the original backing store's size will be used if possible. Note this only applies to the rootfs. For any other filesystems, the original size will be duplicated.
  , cloneHookArgs     :: [String]       -- ^ Additional arguments to pass to the clone hook script.
  }

-- | LXC environment policy.
data AttachEnvPolicy
  = AttachKeepEnv     -- ^ Retain the environment.
  | AttachClearEnv    -- ^ Clear the environment.
  deriving (Eq, Show)

-- | LXC attach options for 'System.LxcOps.attach'.
--
data AttachOptions = AttachOptions
  {
  -- { attachFlags         :: [AttachFlag]       -- ^ Any combination of 'AttachFlag' flags.
  -- , attachNamespaces    :: Int                -- ^ The namespaces to attach to (CLONE_NEW... flags).
  -- -- | Initial personality (@Nothing@ to autodetect).
  -- --
  -- -- * This may be ignored if @lxc@ is compiled without personality support
  -- , attachPersonality   :: Maybe Int64
  -- -- | Inital current directory, @Nothing@ to use @cwd@.
  -- --
  -- -- If the current directory does not exist in the container, the
  -- -- root directory will be used instead because of kernel defaults.
  -- , attachInitialCWD    :: Maybe FilePath
  -- -- | The user-id to run as.
  -- --
  -- -- * /NOTE:/ Set to @-1@ for default behaviour (init uid for userns
  -- -- containers or @0@ (super-user) if detection fails).
  -- , attachUID           :: UserID
  -- -- |The group-id to run as.
  -- --
  -- -- * /NOTE:/ Set to @-1@ for default behaviour (init gid for userns
  -- -- containers or @0@ (super-user) if detection fails).
  -- , attachGID           :: GroupID
    attachEnvPolicy     :: AttachEnvPolicy    -- ^ Environment policy.
  -- , attachExtraEnvVars  :: [String]           -- ^ Extra environment variables to set in the container environment.
  -- , attachExtraKeepEnv  :: [String]           -- ^ Names of environment variables in existing environment to retain in container environment.
  , attachStdinFD       :: Fd                 -- ^ @stdin@ file descriptor.
  -- , attachStdoutFD      :: Fd                 -- ^ @stdout@ file descriptor.
  -- , attachStderrFD      :: Fd                 -- ^ @stderr@ file descriptor.
  }
  deriving (Show)


-- | Default attach options to use.
defaultAttachOptions :: AttachOptions
defaultAttachOptions = AttachOptions
  {
  -- { attachFlags         = [AttachDefault]
  -- , attachNamespaces    = -1
  -- , attachPersonality   = Nothing
  -- , attachInitialCWD    = Nothing
  -- , attachUID           = -1
  -- , attachGID           = -1
    attachEnvPolicy     = AttachKeepEnv
  -- , attachExtraEnvVars  = []
  -- , attachExtraKeepEnv  = []
  , attachStdinFD       = 0
  -- , attachStdoutFD      = 1
  -- , attachStderrFD      = 2
  }
