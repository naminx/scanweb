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
#if __GLASSGLOW_HASKELL__ >= 902
    [absdir|/mnt/m/Documents/Comics/|]
#else
    [absdir|/home/runner/scanweb/comics/|]
#endif


userDataDir :: IsString s => s
userDataDir =
    fromString $ fromAbsDir userDataDir'
  where
    userDataDir' =
#if __GLASSGLOW_HASKELL__ >= 902
        -- [absdir|/home/namin/.config/chromium/|]
        [absdir|/home/namin/.config/google-chrome|]
#else
        [absdir|/home/runner/scanweb/.config/chromium/|]
#endif


chromeExe :: FilePath
chromeExe =
    fromString $ fromAbsFile chromeExe'
  where
    chromeExe' =
#if __GLASSGLOW_HASKELL__ >= 902
        -- [absfile|/run/current-system/sw/bin/chromium|]
        [absfile|/run/current-system/sw/bin/google-chrome-stable|]
#else
        [absfile|/nix/store/ia69plrrvn7czdhn3flq1ll39i92ixab-chromium-92.0.4515.159/bin/chromium|]
#endif


chromeDriverExe :: FilePath
chromeDriverExe =
    fromString $ fromAbsFile chromeDriverExe'
  where
    chromeDriverExe' =
#if __GLASSGLOW_HASKELL__ >= 902
      [absfile|/run/current-system/sw/bin/chromedriver|]
#else
      [absfile|/nix/store/3w27rhw8rwxhf915b7gqlflf02cnqbjv-chromedriver-92.0.4515.107/bin/chromedriver|]
#endif

#endif
