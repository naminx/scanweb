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
    [absdir|/mnt/n/Documents/Comics/|]


userDataDir :: IsString s => s
userDataDir =
    fromString $
        fromAbsDir
            [absdir|/home/namin/.config/chromium/|]


chromeExe :: FilePath
chromeExe =
    fromString $
        fromAbsFile
            [absfile|/nix/store/h45fj64wjszcbc160pr2f32lgdci6kry-chromium-dev-104.0.5112.12/bin/chromium|]

#endif
