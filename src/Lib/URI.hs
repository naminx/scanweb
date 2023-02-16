{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.URI where

import Control.Lens (Choice, Traversal', prism', set, (.~), (?~), _Right)
import RIO hiding (set, (.~))
import Text.URI (
    Authority (Authority),
    RText,
    RTextLabel (Host),
    URI,
    mkURI,
    render,
 )
import Text.URI.Lens (authHost, uriAuthority, uriScheme)
import Text.URI.QQ (scheme)


type Domain = RText 'Host


https :: Domain -> URI -> URI
https host url =
    url
        & uriScheme ?~ [scheme|https|]
        & uriAuthority .~ Right (Authority Nothing host Nothing)


domain :: Traversal' URI Domain
domain = uriAuthority . _Right . authHost


absPath :: URI -> URI
absPath = set uriScheme Nothing . set uriAuthority (Left True)


relPath :: URI -> URI
relPath = set uriScheme Nothing . set uriAuthority (Left False)


toURI :: (Choice p, Applicative f) => p URI (f URI) -> p Text (f Text)
toURI = prism' render mkURI
