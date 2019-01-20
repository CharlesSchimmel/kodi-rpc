{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Types.Fields.All where

import KodiRPC.Types.Base

import Data.Aeson hiding (Result, Error)
import Data.Aeson.Types hiding (Result, Error)
import Data.Text as T

data AllField = Album | Albumartist        | Albumartistid  | Albumid                  | Albumlabel
                       | Albumreleasetype   | Art            | Artist                   | Artistid           | Cast
                       | Channel            | Channelnumber  | Channeltype              | Comment            | Compilation
                       | Contributors       | Country        | Dateadded                | Description        | Director
                       | Disc               | Displayartist  | Displaycomposer          | Displayconductor   | Displaylyricist
                       | Displayorchestra   | Duration       | Endtime                  | Episode            | Episodeguide
                       | Fanart             | File           | Firstaired               | Genre              | Genreid
                       | Hidden             | Imdbnumber     | Lastplayed               | Locked             | Lyrics
                       | Mood               | Mpaa           | Musicbrainzalbumartistid | Musicbrainzalbumid | Musicbrainzartistid
                       | Musicbrainztrackid | Originaltitle  | Playcount                | Plot               | Plotoutline
                       | Premiered          | Productioncode | Rating                   | Releasetype        | Resume
                       | Runtime            | Season         | Set                      | Setid              | Showlink
                       | Showtitle          | Sorttitle      | Specialsortepisode       | Specialsortseason  | Starttime
                       | Streamdetails      | Studio         | Style                    | Tag                | Tagline
                       | Theme              | Thumbnail      | Title                    | Top250             | Track
                       | Trailer            | Tvshowid       | Uniqueid                 | Userrating         | Votes
                       | Watchedepisodes    | Writer         | Year
                       deriving ( Show )

instance Field AllField where
  fromField = T.unpack . T.toLower . T.pack . show
