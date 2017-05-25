{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.API.TheMovieDB.Actions
       ( searchMovies
       , searchMoviesLocalized
       , fetchMovie
       , fetchMovieLocalized
       , searchTV
       , searchTVLocalized
       , fetchTV
       , fetchTVLocalized
       , fetchTVSeason
       , fetchTVSeasonLocalized
       , fetchFullTVSeries
       , fetchFullTVSeriesLocalized
       , config
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Text (Text)
import Network.API.TheMovieDB.Internal.Configuration
import Network.API.TheMovieDB.Internal.SearchResults
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Internal.Types
import Network.API.TheMovieDB.Types.Movie
import Network.API.TheMovieDB.Types.Season
import Network.API.TheMovieDB.Types.TV

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Search TheMovieDB using the given query string.
--
-- The movies returned will not have all their fields completely
-- filled out, to get a complete record you'll need to follow this
-- call up with a call to 'fetchMovie'.
searchMovies :: Text -> TheMovieDB [Movie]
searchMovies query = searchResults <$> search
  where search = getAndParse "search/movie" [("query", Just query)]

--------------------------------------------------------------------------------
-- | Search TheMovieDB using the given query string.
--
-- The movies returned will not have all their fields completely
-- filled out, to get a complete record you'll need to follow this
-- call up with a call to 'fetchMovie'.
searchMoviesLocalized :: Text -> Language -> TheMovieDB [Movie]
searchMoviesLocalized query language = searchResults <$> search
  where search = getAndParse "search/movie" [("query", Just query), ("language", Just language)]

--------------------------------------------------------------------------------
-- | Fetch the metadata for the 'Movie' with the given ID.
fetchMovie :: ItemID -- ^ TheMovieDB ID for the movie.
           -> TheMovieDB Movie
fetchMovie mid = getAndParse ("movie/" ++ show mid) []

--------------------------------------------------------------------------------
-- | Fetch the metadata for the 'Movie' with the given ID.
fetchMovieLocalized :: ItemID -- ^ TheMovieDB ID for the movie.
           -> Language
           -> TheMovieDB Movie
fetchMovieLocalized mid language = getAndParse ("movie/" ++ show mid) [("language", Just language)]

--------------------------------------------------------------------------------
-- | Search TheMovieDB for matching 'TV' series.
--
-- The 'TV' values returned from this function will be partial
-- records.  The only fields that will be available are 'tvID',
-- 'tvName', 'tvPosterPath', 'tvPopularity', and possibly
-- 'tvFirstAirDate'.
--
-- To get full 'TV' records you need to follow this function with a
-- call to 'fetchTV' using the desired 'tvID' value.
searchTV :: Text -> TheMovieDB [TV]
searchTV query = searchResults <$> search
  where search = getAndParse "search/tv" [("query", Just query)]

--------------------------------------------------------------------------------
-- | Search TheMovieDB for matching 'TV' series.
--
-- The 'TV' values returned from this function will be partial
-- records.  The only fields that will be available are 'tvID',
-- 'tvName', 'tvPosterPath', 'tvPopularity', and possibly
-- 'tvFirstAirDate'.
--
-- To get full 'TV' records you need to follow this function with a
-- call to 'fetchTV' using the desired 'tvID' value.
searchTVLocalized :: Text -> Language -> TheMovieDB [TV]
searchTVLocalized query language = searchResults <$> search
  where search = getAndParse "search/tv" [("query", Just query), ("language", Just language)]

--------------------------------------------------------------------------------
-- | Fetch metadata for a 'TV' series given its TheMovieDB ID.  The
-- metadata for 'Season's listed in the TV series will not have
-- complete 'Episode' information.
--
-- After calling this function you should call 'fetchTVSeason' to fill
-- in the 'Episode' metadata, or just begin with 'fetchFullTVSeries'.
fetchTV :: ItemID -- ^ TheMovieDB ID for the TV series.
        -> TheMovieDB TV
fetchTV i = getAndParse ("tv/" ++ show i) []

--------------------------------------------------------------------------------
-- | Fetch metadata for a 'TV' series given its TheMovieDB ID.  The
-- metadata for 'Season's listed in the TV series will not have
-- complete 'Episode' information.
--
-- After calling this function you should call 'fetchTVSeason' to fill
-- in the 'Episode' metadata, or just begin with 'fetchFullTVSeries'.
fetchTVLocalized :: ItemID -- ^ TheMovieDB ID for the TV series.
        -> Language
        -> TheMovieDB TV
fetchTVLocalized i language = getAndParse ("tv/" ++ show i) [("language", Just language)]


--------------------------------------------------------------------------------
-- | Fetch metadata for a 'Season', including all 'Episode's.
fetchTVSeason :: ItemID         -- ^ TheMovieDB ID for the TV series.
              -> Int            -- ^ Season number (not season ID).
              -> TheMovieDB Season
fetchTVSeason i n = getAndParse ("tv/" ++ show i ++ "/season/" ++ show n) []

--------------------------------------------------------------------------------
-- | Fetch metadata for a 'Season', including all 'Episode's.
fetchTVSeasonLocalized :: ItemID         -- ^ TheMovieDB ID for the TV series.
              -> Int            -- ^ Season number (not season ID).
              -> Language
              -> TheMovieDB Season
fetchTVSeasonLocalized i n language = getAndParse ("tv/" ++ show i ++ "/season/" ++ show n) [("language", Just language)]


--------------------------------------------------------------------------------
-- | Fetch full metadata for a 'TV' series, including all seasons and
-- episodes.
--
-- This function will make multiple HTTP requests to TheMovieDB API.
fetchFullTVSeries :: ItemID -- ^ TheMovieDB ID for the TV series.
                  -> TheMovieDB TV
fetchFullTVSeries i = do
  tv      <- fetchTV i
  seasons <- mapM (fetchTVSeason i . seasonNumber) (tvSeasons tv)
  return tv {tvSeasons = seasons}

-- | Fetch full metadata for a 'TV' series, including all seasons and
-- episodes.
--
-- This function will make multiple HTTP requests to TheMovieDB API.
fetchFullTVSeriesLocalized :: ItemID -- ^ TheMovieDB ID for the TV series.
                  -> Language
                  -> TheMovieDB TV
fetchFullTVSeriesLocalized i language = do
  tv      <- fetchTVLocalized i language
  seasons <- mapM (\season -> fetchTVSeasonLocalized i (seasonNumber season) language) (tvSeasons tv)
  return tv {tvSeasons = seasons}


--------------------------------------------------------------------------------
-- | Fetch the API configuration information such as base URLs for
-- movie posters.  The resulting configuration value should be cached
-- and only requested every few days.
config :: TheMovieDB Configuration
config = getAndParse "configuration" []
