{-# LANGUAGE CPP #-}
#if defined(mingw32_HOST_OS)
#else
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
#endif
{-# LANGUAGE NoImplicitPrelude #-}

module App.Config.Linux where


#if defined(mingw32_HOST_OS)
#else

import Path (
    Abs,
    Dir,
    Path,
    absdir,
    absfile,
    fromAbsDir,
    fromAbsFile,
 )
import RIO


defaultRootDir :: Path Abs Dir
defaultRootDir =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,7,0)
    [absdir|/mnt/m/Documents/Comics/|]
#else
    [absdir|/home/runner/scanweb/comics/|]
#endif


userDataDir :: IsString s => s
userDataDir =
    fromString $ fromAbsDir userDataDir'
  where
    userDataDir' =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,7,0)
        [absdir|/home/namin/.config/google-chrome|]
#else
        [absdir|/home/runner/scanweb/.config/google-chrome/|]
#endif


chromeExe :: FilePath
chromeExe =
    fromString $ fromAbsFile chromeExe'
  where
    chromeExe' =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,7,0)
        [absfile|/run/current-system/sw/bin/google-chrome-stable|]
#else
        [absfile|/nix/store/mfcdk6vvhr3sb8pxz5nf587gqjsrh1d7-google-chrome-110.0.5481.100/bin/google-chrome-stable|]
#endif


chromeDriverExe :: FilePath
chromeDriverExe =
    fromString $ fromAbsFile chromeDriverExe'
  where
    chromeDriverExe' =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,7,0)
        [absfile|/run/current-system/sw/bin/chromedriver|]
#else
        [absfile|/nix/store/ls1wv7vwn3j6v9micys4z5i1g143kjb7-chromedriver-110.0.5481.77/bin/chromedriver|]
#endif

#endif
