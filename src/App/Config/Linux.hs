{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Config.Linux where


#if defined(mingw32_HOST_OS)
#else

import Path (
    Abs,
    Dir,
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
#else
    File,
#endif
    Path,
    absdir,
    absfile,
    fromAbsDir,
    fromAbsFile,
 )
import RIO


defaultRootDir :: Path Abs Dir
defaultRootDir =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
    [absdir|/mnt/m/Documents/Comics/|]
#else
    [absdir|/home/runner/scanweb/comics/|]
#endif


userDataDir :: IsString s => s
userDataDir =
    fromString $ fromAbsDir userDataDir'
  where
    userDataDir' =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
        [absdir|/home/namin/.config/google-chrome|]
#else
        [absdir|/home/runner/scanweb/.config/google-chrome/|]
#endif


chromeExe :: FilePath
chromeExe =
    fromString $ fromAbsFile chromeExe'
  where
    chromeExe' =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
        [absfile|/run/current-system/sw/bin/google-chrome-stable|]
#else
        [absfile|/nix/store/vmnmzm12zzqbr73kkbz213zf9d0qv0q8-google-chrome-92.0.4515.159/bin/google-chrome-stable|]
#endif


chromeDriverExe :: FilePath
chromeDriverExe =
    fromString $ fromAbsFile chromeDriverExe'
  where
    chromeDriverExe' =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
        [absfile|/run/current-system/sw/bin/chromedriver|]
#else
        [absfile|/nix/store/3w27rhw8rwxhf915b7gqlflf02cnqbjv-chromedriver-92.0.4515.107/bin/chromedriver|]
#endif

#endif
