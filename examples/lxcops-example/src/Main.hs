module Main where

-- Compile and run this:
-- ```
-- stack build
-- stack install
-- sudo lxcops-example
-- ```

import Data.Monoid
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import System.LxcOps
import System.LxcOps.Types

lxcAttachOptions :: AttachOptions
lxcAttachOptions = defaultAttachOptions { attachEnvPolicy = AttachClearEnv }

main = do

  withContainer (Container "container-name" Nothing) $ do

    isdef <- isDefined
    when isdef $ do

      ci <- containerInfo
      liftIO $ putStrLn $ "Container exists: " <> show ci

      stop
      wait ContainerStopped (-1)
      destroy
      pure ()

    d <- create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "xenial", "-a", "amd64"]
    start False []
    wait ContainerRunning (-1)

    cmdresult <- attachRunWait lxcAttachOptions "uname" ["-a"]
    liftIO $ putStrLn $ "uname result: " <> show cmdresult

    stop
    wait ContainerStopped (-1)
    clone (Just "new-name") Nothing [CloneSnapshot] Nothing Nothing Nothing []

  withContainer (Container "new-name" Nothing) $ do
      ci <- containerInfo
      liftIO $ putStrLn $ "Clone: " <> show ci
