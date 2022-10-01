{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Config.Linux where

#if defined(mingw32_HOST_OS)
#else

import Path (Abs, Dir, Path, absdir, absfile, fromAbsDir, fromAbsFile)
import RIO


defaultRootDir :: Path Abs Dir
defaultRootDir =
    [absdir|/mnt/m/Documents/Comics/|]


userDataDir :: IsString s => s
userDataDir =
    fromString $
        fromAbsDir
--          [absdir|/home/namin/.config/chromium/|]
            [absdir|/home/namin/.config/google-chrome|]


chromeExe :: FilePath
chromeExe =
    fromString $
        fromAbsFile
--          [absfile|/run/current-system/sw/bin/chromium|]
            [absfile|/run/current-system/sw/bin/google-chrome-stable|]


#endif
