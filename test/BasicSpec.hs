module BasicSpec (main, spec) where

import System.LxcOps.Types
import System.LxcOps.Wrapper
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Text.InterpolatedString.Perl6 (q)

spec :: Spec
spec = do

  describe "Tests function parseContainerInfo" $ do

    it "valid example" $
        parseContainerInfo cinfo
            `shouldBe` (Just $ ContainerInfo "disp" ContainerRunning (Just "192.168.122.93"))

    it "test stopped container" $
        parseContainerInfo cstopinfo
            `shouldBe` (Just $ ContainerInfo "proto" ContainerStopped Nothing)

main :: IO ()
main = hspec spec

cinfo = [q|
Name:           disp
State:          RUNNING
PID:            1611
IP:             192.168.122.93
CPU use:        89.87 seconds
BlkIO use:      5.08 MiB
Memory use:     127.50 MiB
KMem use:       18.89 MiB
Link:           vethCLTEXI
 TX bytes:      3.78 MiB
 RX bytes:      18.61 MiB
 Total bytes:   22.39 MiB
|]

cstopinfo = [q|
Name:           proto
State:          STOPPED
|]
