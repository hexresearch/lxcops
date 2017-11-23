module System.LxcOps.Wrapper where

import Control.Applicative
import Control.Applicative.Combinators (skipManyTill)
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Function
import Data.IP
import Data.Maybe
import Data.Monoid
import Data.Word
import HSH hiding (space)
import HSH.Command
import Safe (headMay, tailSafe)
import System.Exit
import System.Process

import Text.InterpolatedString.Perl6 (qc)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

import System.Posix.IO (fdToHandle)
import System.LxcOps.Types
import System.LxcOps.FreeDsl

-- | Get some information about a container with specified name
--
lxcIsDefined :: String -> IO Bool
lxcIsDefined name = run ("lxc-info", ["-n", name])

-- | Get some information about a container with specified name
--
lxcInfo :: String -> IO (Maybe ContainerInfo)
lxcInfo name = do
  (raw, iostatus) <- run $ ("lxc-info", ["-n", name])
  (_, status) :: (String, ExitCode) <- iostatus
  pure $ case status of
        ExitFailure _ -> Nothing
        ExitSuccess -> parseContainerInfo raw

-- | Parse output of console command lxc-info -n <name>
--
-- Name:           disp
-- State:          RUNNING
-- PID:            1611
-- IP:             192.168.122.93
-- CPU use:        89.87 seconds
-- BlkIO use:      5.08 MiB
-- Memory use:     127.50 MiB
-- KMem use:       18.89 MiB
-- Link:           vethCLTEXI
--  TX bytes:      3.78 MiB
--  RX bytes:      18.61 MiB
--  Total bytes:   22.39 MiB
parseContainerInfo :: String -> Maybe ContainerInfo
parseContainerInfo = either (const Nothing) Just . runParser p ""
 where
  p :: Parsec Dec String ContainerInfo
  p = do
    space
    ciName  <- kv "Name:" $ someTill anyChar eol
    space
    ciState <- kv "State:" $ lxcStateP
    ciIP <- try ((Just . read) <$> do
        space
        -- _ <- kv "PID:" $ manyTill digitChar eol
        _ <- manyTill anyChar eol
        space
        kv "IP:" parseIPv4
      ) <|> pure Nothing
    pure ContainerInfo {..}

  lxcStateP = flip fmap (some alphaNumChar) $ \case
    "STOPPED" -> ContainerStopped
    "STARTING" -> ContainerStarting
    "RUNNING" -> ContainerRunning
    "STOPPING" -> ContainerStopping
    "ABORTING" -> ContainerAborting
    "FREEZING" -> ContainerFreezing
    "FROZEN" -> ContainerFrozen
    "THAWED" -> ContainerThawed
    s -> ContainerOtherState s

kv :: String -> Parsec Dec String a -> Parsec Dec String a
kv x vp = string x >> skipSome spaceChar >> vp

-- | Call `lxc-create` command
--
lxcCreate :: CreateParams -> IO Bool -- ^ @True@ on success. @False@ otherwise.
lxcCreate CreateParams {..} =
    run ("lxc-create", ["-t", createTemplate, "-n", createName, "--"] <> createArgs)

-- | Call `lxc-start` command
--
lxcStart :: String -> Bool -> [String] -> IO Bool
lxcStart name useinit argv = run ("lxc-start", ["-n", name])

-- | Call `lxc-wait` command
--
lxcWait :: String -> [ContainerState] -> Maybe Int -> IO Bool
-- lxcWait name ss mt = do
    -- let waitCommand = ("lxc-wait", ["-n", name] <> args)
    -- putStrLn $ "Wait command: " <> show waitCommand
    -- run waitCommand
  -- where
    -- args = join $ ((\s -> ["-s", s]) <$> filter (not . null) [intercalate "|" (showState <$> ss)])
    --            <> ((\t -> ["-t", show t]) <$> maybeToList mt)
lxcWait name ss mt = do
  let loop = do
        mi <- lxcInfo name
        if (maybe False id $ (elem . ciState) <$> mi <*> Just ss)
            then (pure True)
            else do
                putStrLn $ [qc|Waiting for container {show name} state is in {ss}. Now: {mi}|]
                threadDelay $ 10^6
                loop
  loop

-- | Call `lxc-stop` command
--
lxcStop :: String -> IO Bool
lxcStop name = run ("lxc-stop", ["-n", name])

-- | Call `lxc-destroy` command
--
lxcDestroy :: String -> IO Bool
lxcDestroy name = run ("lxc-destroy", ["-n", name])

lxcAttachRun' :: (RunResult r) => String -> AttachOptions -> String -> [String] -> r
lxcAttachRun' name ops prg argv =
    run $ lxcAttachRunToRawCommand name ops prg argv

lxcAttachRunToRawCommand
  :: String -> AttachOptions -> String -> [String] -> (String, [String])
lxcAttachRunToRawCommand name AttachOptions {..} prg argv =
  ("lxc-attach", ["-n", name, envKey, "--", prg] <> argv)
 where
  envKey = case attachEnvPolicy of
    AttachKeepEnv  -> "--keep-env"
    AttachClearEnv -> "--clear-env"

lxcAttachRunWait :: String -> AttachOptions -> String -> [String] -> IO (Maybe ExitCode)
lxcAttachRunWait name ops prg argv = do
    let (prg', args') = lxcAttachRunToRawCommand name ops prg argv
    let cp = (proc prg' args') { delegate_ctlc = True }
    let inFD = attachStdinFD ops
    cp' <- do
        -- Тут получилось костыльно: если передано значение файл-дескриптора
        -- оличное от значения по умолчанию, то делаем из него Handle и
        -- передаём в процесс
        if inFD /= attachStdinFD defaultAttachOptions
          then do
            inh <- fdToHandle inFD
            pure $ cp { std_in = UseHandle inh }
          else pure cp

    exitCode <- withCreateProcess cp' $ \_ _ _ p ->
                   waitForProcess p
    pure $ Just exitCode

lxcGetInterfaces :: String -> IO [String]
lxcGetInterfaces name =
    catMaybes . fmap (headMay . words) . tailSafe
        <$> lxcAttachRun' name aops "ifconfig" ["-s"]
  where
    aops = defaultAttachOptions { attachEnvPolicy = AttachClearEnv }

lxcGetIPs :: String -> String -> String -> Word32 -> IO [String]
lxcGetIPs name iface fam sid = do
    (str, iostatus) :: (String, IO (String, ExitCode))
        <- lxcAttachRun' name aops "ifconfig" [iface]
    (_, status) :: (String, ExitCode) <- iostatus
    pure $ case status of
          ExitFailure _ -> []
          ExitSuccess -> str
            & lines
            & fmap words
            & filter ((==Just fam) . headMay)
            & mconcat . fmap (maybeToList . headMay . tailSafe)
            & mconcat . fmap (maybeToList . extractIPv4)
  where
    aops = defaultAttachOptions { attachEnvPolicy = AttachClearEnv }

-- | This is actual only for `dir` backend
lxcClone :: CloneParams -> IO (Maybe Container)
lxcClone CloneParams {..} = do
    let copyCommand =
          ( "lxc-copy"
          , ["-n", cloneOriginalName]
         <> (if elem CloneSnapshot cloneOps then ["-s"] else [])
         <> consparam "--newname" cloneMNewName
          )
    putStrLn [qc|Copy command: {copyCommand}|]
    whenMaybe (Container newName Nothing) <$> run copyCommand
  where
    consparam :: String -> Maybe String -> [String]
    consparam pname = maybe [] (\v -> [pname, v])

    newName = fromMaybe cloneOriginalName cloneMNewName

whenMaybe _ False = Nothing
whenMaybe a True = Just a

extractIPv4 :: String -> Maybe String
extractIPv4 = either (const Nothing) Just . runParser p ""
 where
  p :: Parsec Dec String String
  p = skipManyTill anyChar parseIPv4 <* many anyChar

parseIPv4 :: Parsec Dec String String
parseIPv4 = do
  a <- some digitChar
  char '.'
  b <- some digitChar
  char '.'
  c <- some digitChar
  char '.'
  d <- some digitChar
  pure $ a <> "." <> b <> "." <> c <> "." <> d
