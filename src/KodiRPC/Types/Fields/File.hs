{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Types.Fields.File where

import KodiRPC.Types.Base

import Data.Aeson hiding (Result, Error)
import Data.Aeson.Types hiding (Result, Error)
import Data.Text as T

data FileField = Album | Albumartist    | Albumartistid            | Albumid            | Albumlabel
                       | Art            | Artist                   | Artistid           | Cast                | Comment
                       | Country        | Dateadded                | Description        | Director            | Disc
                       | Displayartist  | Duration                 | Episode            | Episodeguide        | Fanart
                       | File           | Firstaired               | Genre              | Genreid             | Imdbnumber
                       | Lastmodified   | Lastplayed               | Lyrics             | Mimetype            | Mood
                       | Mpaa           | Musicbrainzalbumartistid | Musicbrainzalbumid | Musicbrainzartistid | Musicbrainztrackid
                       | Originaltitle  | Playcount                | Plot               | Plotoutline         | Premiered
                       | Productioncode | Rating                   | Resume             | Runtime             | Season
                       | Set            | Setid                    | Showlink           | Showtitle           | Size
                       | Sorttitle      | Specialsortepisode       | Specialsortseason  | Streamdetails       | Studio
                       | Style          | Tag                      | Tagline            | Theme               | Thumbnail
                       | Title          | Top250                   | Track              | Trailer             | Tvshowid
                       | Uniqueid       | Votes                    | Watchedepisodes    | Writer              | Year
  deriving ( Show )

instance Field FileField where
  fromField = T.unpack . T.toLower . T.pack . show
