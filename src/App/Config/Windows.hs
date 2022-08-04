{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Config.Windows where


#if defined(mingw32_HOST_OS)

import Path (Abs, Dir, File, Path, absdir, absfile, fromAbsDir, fromAbsFile)
import RIO


defaultRootDir :: Path Abs Dir
defaultRootDir =
    [absdir|N:\Documents\Comics\|]


userDataDir :: IsString s => s
userDataDir =
    fromString $
        fromAbsDir
            [absdir|C:\Users\namin\AppData\Local\Google\Chrome\User Data\|]


chromeExe :: FilePath
chromeExe =
    fromString $
        fromAbsFile
            [absfile|C:\Program Files\Google\Chrome\Application\chrome.exe|]

#else
#endif
