{-# LANGUAGE OverloadedStrings #-}

module KodiRPC.Methods.Input where

import KodiRPC.Types.Base

import Prelude as P
import Data.HashMap.Strict as HM
import Data.Text as T
import Data.Aeson.Types hiding (Error)

contextMenu           = methodNoP "Input.ContextMenu"
home                  = methodNoP "Input.Home"
info                  = methodNoP "Input.Info"
select                = methodNoP "Input.Select"
showCodec             = methodNoP "Input.ShowCodec"
showOsd               = methodNoP "Input.ShowOSD"
showPlayerProcessInfo = methodNoP "Input.ShowPlayerProcessInfo"
back                  = methodNoP "Input.Back"
down                  = methodNoP "Input.Down"
up                    = methodNoP "Input.Up"
left                  = methodNoP "Input.Left"
right                 = methodNoP "Input.Right"
sendText :: Text -> Method
sendText              = method'   "Input.SendText"      . HM.singleton "text" . String
executeAction :: Action -> Method
executeAction         = method'   "Input.ExecuteAction" . HM.singleton "action" . showAction

data Action = Left | Right                 | Up                 | Down                | PageUp                  | PageDown
                   | Select                | Highlight          | Parentdir           | Parentfolder            | Back
                   | Menu                  | Previousmenu       | Info                | Pause                   | Stop
                   | Skipnext              | Skipprevious       | Fullscreen          | Aspectratio             | Stepforward
                   | Stepback              | Bigstepforward     | Bigstepback         | Chapterorbigstepforward | Chapterorbigstepback
                   | Osd                   | Showsubtitles      | Nextsubtitle        | Cyclesubtitle           | Playerdebug
                   | Codecinfo             | Playerprocessinfo  | Nextpicture         | Previouspicture         | Zoomout
                   | Zoomin                | Playlist           | Queue               | Zoomnormal              | Zoomlevel1
                   | Zoomlevel2            | Zoomlevel3         | Zoomlevel4          | Zoomlevel5              | Zoomlevel6
                   | Zoomlevel7            | Zoomlevel8         | Zoomlevel9          | Nextcalibration         | Resetcalibration
                   | Analogmove            | Analogmovex        | Analogmovey         | Rotate                  | Rotateccw
                   | Close                 | Subtitledelayminus | Subtitledelay       | Subtitledelayplus       | Audiodelayminus
                   | Audiodelay            | Audiodelayplus     | Subtitleshiftup     | Subtitleshiftdown       | Subtitlealign
                   | Audionextlanguage     | Verticalshiftup    | Verticalshiftdown   | Nextresolution          | Audiotoggledigital
                   | Number0               | Number1            | Number2             | Number3                 | Number4
                   | Number5               | Number6            | Number7             | Number8                 | Number9
                   | Smallstepback         | Fastforward        | Rewind              | Play                    | Playpause
                   | Switchplayer          | Delete             | Copy                | Move                    | Screenshot
                   | Rename                | Togglewatched      | Scanitem            | Reloadkeymaps           | Volumeup
                   | Volumedown            | Mute               | Backspace           | Scrollup                | Scrolldown
                   | Analogfastforward     | Analogrewind       | Moveitemup          | Moveitemdown            | Contextmenu
                   | Shift                 | Symbols            | Cursorleft          | Cursorright             | Showtime
                   | Analogseekforward     | Analogseekback     | Showpreset          | Nextpreset              | Previouspreset
                   | Lockpreset            | Randompreset       | Increasevisrating   | Decreasevisrating       | Showvideomenu
                   | Enter                 | Increaserating     | Decreaserating      | Setrating               | Togglefullscreen
                   | Nextscene             | Previousscene      | Nextletter          | Prevletter              | Jumpsms2
                   | Jumpsms3              | Jumpsms4           | Jumpsms5            | Jumpsms6                | Jumpsms7
                   | Jumpsms8              | Jumpsms9           | Filter              | Filterclear             | Filtersms2
                   | Filtersms3            | Filtersms4         | Filtersms5          | Filtersms6              | Filtersms7
                   | Filtersms8            | Filtersms9         | Firstpage           | Lastpage                | Guiprofile
                   | Red                   | Green              | Yellow              | Blue                    | Increasepar
                   | Decreasepar           | Volampup           | Volampdown          | Volumeamplification     | Createbookmark
                   | Createepisodebookmark | Settingsreset      | Settingslevelchange | Stereomode              | Nextstereomode
                   | Previousstereomode    | Togglestereomode   | Stereomodetomono    | Channelup               | Channeldown
                   | Previouschannelgroup  | Nextchannelgroup   | Playpvr             | Playpvrtv               | Playpvrradio
                   | Record                | Togglecommskip     | Showtimerrule       | Leftclick               | Rightclick
                   | Middleclick           | Doubleclick        | Longclick           | Wheelup                 | Wheeldown
                   | Mousedrag             | Mousemove          | Tap                 | Longpress               | Pangesture
                   | Zoomgesture           | Rotategesture      | Swipeleft           | Swiperight              | Swipeup
                   | Swipedown             | Error              | Noop
            deriving (Show, Read) 

showAction :: Action -> Value
showAction = String . T.toLower . pack . show
