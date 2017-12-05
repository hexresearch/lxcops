# lxcops

Это обёртка над консольными командами lxc.
Интерфейс — как в библиотеке https://hackage.haskell.org/package/lxc, но без
использования `bindings-lxc`, и реализован только частично, по необходимости.

#### Пример использования

```haskell
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import System.LxcOps
import System.LxcOps.Types

lxcAttachOptions :: AttachOptions
lxcAttachOptions = defaultAttachOptions { attachEnvPolicy = AttachClearEnv }

withContainer (Container "container-name" Nothing) $ do

  isdef <- isDefined
  when isdef $ do

    ci <- containerInfo
    liftIO $ putStrLn $ "Container exists: " <> show ci

    stop
    wait ContainerStopped (-1)
    destroy

  d <- create "download" Nothing Nothing [] ["-d", "ubuntu", "-r", "xenial", "-a", "amd64"]
  start False []
  wait ContainerRunning (-1)

  cmdresult <- attachRunWait lxcAttachOptions "uname" ["-a"]
  liftIO $ putStrLn $ "uname result: " <> show cmdresult

  clone (Just "new-name") Nothing [CloneSnapshot] Nothing Nothing Nothing []

withContainer (Container "new-name" Nothing) $ do
    ci <- containerInfo
    liftIO $ putStrLn $ "Clone: " <> show ci
```
