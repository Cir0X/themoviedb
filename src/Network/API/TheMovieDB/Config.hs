{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.API.TheMovieDB.Config
       ( configErr
       , config
       ) where

--------------------------------------------------------------------------------
import Network.API.TheMovieDB.Internal.Generic
import Network.API.TheMovieDB.Types

--------------------------------------------------------------------------------
-- | Fetch the API configuration information such as base URLs for
-- movie posters.  Results in either an 'Error' or a 'Configuration'.
configErr :: TheMovieDB (Either Error Configuration)
configErr = getAndParse "configuration" []

--------------------------------------------------------------------------------
-- | Fetch the API configuration information or fail.  For a function
-- that returns an error instead of failing see 'configErr'.
config :: TheMovieDB Configuration
config = getOrFail configErr
