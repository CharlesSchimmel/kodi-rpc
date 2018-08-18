{-# LANGUAGE OverloadedStrings #-}

module Methods.Input where

import Types
import Calls
import Data.HashMap.Strict as HM
import Data.Text
import Data.Aeson.Types hiding (Error)

method' = Method 1.0 2.0
methodNoP x = method' x emptyObject

back                  = methodNoP   "Input.Back"
contextMenu           = methodNoP   "Input.ContextMenu"
down                  = methodNoP   "Input.Down"
executeAction :: Action -> Method
executeAction action  = method'     "Input.ExecuteAction" $ Object . HM.singleton "action" $ actionStr action
home                  = methodNoP   "Input.Home"
info                  = methodNoP   "Input.Info"
left                  = methodNoP   "Input.Left"
right                 = methodNoP   "Input.Right"
select                = methodNoP   "Input.Select"
sendText :: Text -> Method
sendText str          = method'     "Input.SendText" $ Object . HM.singleton "text" $ String str
showCodec             = methodNoP   "Input.ShowCodec"
showOsd               = methodNoP   "Input.ShowOSD"
showPlayerProcessInfo = methodNoP   "Input.ShowPlayerProcessInfo"
up                    = methodNoP   "Input.Up"

data Action = Left
            | Right
            | Up
            | Down
            | PageUp
            | PageDown
            | Select
            | Highlight
            | Parentdir
            | Parentfolder
            | Back
            | Menu
            | Previousmenu
            | Info
            | Pause
            | Stop
            | Skipnext
            | Skipprevious
            | Fullscreen
            | Aspectratio
            | Stepforward
            | Stepback
            | Bigstepforward
            | Bigstepback
            | Chapterorbigstepforward
            | Chapterorbigstepback
            | Osd
            | Showsubtitles
            | Nextsubtitle
            | Cyclesubtitle
            | Playerdebug
            | Codecinfo
            | Playerprocessinfo
            | Nextpicture
            | Previouspicture
            | Zoomout
            | Zoomin
            | Playlist
            | Queue
            | Zoomnormal
            | Zoomlevel1
            | Zoomlevel2
            | Zoomlevel3
            | Zoomlevel4
            | Zoomlevel5
            | Zoomlevel6
            | Zoomlevel7
            | Zoomlevel8
            | Zoomlevel9
            | Nextcalibration
            | Resetcalibration
            | Analogmove
            | Analogmovex
            | Analogmovey
            | Rotate
            | Rotateccw
            | Close
            | Subtitledelayminus
            | Subtitledelay
            | Subtitledelayplus
            | Audiodelayminus
            | Audiodelay
            | Audiodelayplus
            | Subtitleshiftup
            | Subtitleshiftdown
            | Subtitlealign
            | Audionextlanguage
            | Verticalshiftup
            | Verticalshiftdown
            | Nextresolution
            | Audiotoggledigital
            | Number0
            | Number1
            | Number2
            | Number3
            | Number4
            | Number5
            | Number6
            | Number7
            | Number8
            | Number9
            | Smallstepback
            | Fastforward
            | Rewind
            | Play
            | Playpause
            | Switchplayer
            | Delete
            | Copy
            | Move
            | Screenshot
            | Rename
            | Togglewatched
            | Scanitem
            | Reloadkeymaps
            | Volumeup
            | Volumedown
            | Mute
            | Backspace
            | Scrollup
            | Scrolldown
            | Analogfastforward
            | Analogrewind
            | Moveitemup
            | Moveitemdown
            | Contextmenu
            | Shift
            | Symbols
            | Cursorleft
            | Cursorright
            | Showtime
            | Analogseekforward
            | Analogseekback
            | Showpreset
            | Nextpreset
            | Previouspreset
            | Lockpreset
            | Randompreset
            | Increasevisrating
            | Decreasevisrating
            | Showvideomenu
            | Enter
            | Increaserating
            | Decreaserating
            | Setrating
            | Togglefullscreen
            | Nextscene
            | Previousscene
            | Nextletter
            | Prevletter
            | Jumpsms2
            | Jumpsms3
            | Jumpsms4
            | Jumpsms5
            | Jumpsms6
            | Jumpsms7
            | Jumpsms8
            | Jumpsms9
            | Filter
            | Filterclear
            | Filtersms2
            | Filtersms3
            | Filtersms4
            | Filtersms5
            | Filtersms6
            | Filtersms7
            | Filtersms8
            | Filtersms9
            | Firstpage
            | Lastpage
            | Guiprofile
            | Red
            | Green
            | Yellow
            | Blue
            | Increasepar
            | Decreasepar
            | Volampup
            | Volampdown
            | Volumeamplification
            | Createbookmark
            | Createepisodebookmark
            | Settingsreset
            | Settingslevelchange
            | Stereomode
            | Nextstereomode
            | Previousstereomode
            | Togglestereomode
            | Stereomodetomono
            | Channelup
            | Channeldown
            | Previouschannelgroup
            | Nextchannelgroup
            | Playpvr
            | Playpvrtv
            | Playpvrradio
            | Record
            | Togglecommskip
            | Showtimerrule
            | Leftclick
            | Rightclick
            | Middleclick
            | Doubleclick
            | Longclick
            | Wheelup
            | Wheeldown
            | Mousedrag
            | Mousemove
            | Tap
            | Longpress
            | Pangesture
            | Zoomgesture
            | Rotategesture
            | Swipeleft
            | Swiperight
            | Swipeup
            | Swipedown
            | Error
            | Noop

actionTxt :: Action -> Text
actionTxt Methods.Input.Left      = "left"
actionTxt Methods.Input.Right     = "right"
actionTxt Up                      = "up"
actionTxt Down                    = "down"
actionTxt PageUp                  = "pageUp"
actionTxt PageDown                = "pageDown"
actionTxt Select                  = "select"
actionTxt Highlight               = "highlight"
actionTxt Parentdir               = "parentdir"
actionTxt Parentfolder            = "parentfolder"
actionTxt Back                    = "back"
actionTxt Menu                    = "menu"
actionTxt Previousmenu            = "previousmenu"
actionTxt Info                    = "info"
actionTxt Pause                   = "pause"
actionTxt Stop                    = "stop"
actionTxt Skipnext                = "skipnext"
actionTxt Skipprevious            = "skipprevious"
actionTxt Fullscreen              = "fullscreen"
actionTxt Aspectratio             = "aspectratio"
actionTxt Stepforward             = "stepforward"
actionTxt Stepback                = "stepback"
actionTxt Bigstepforward          = "bigstepforward"
actionTxt Bigstepback             = "bigstepback"
actionTxt Chapterorbigstepforward = "chapterorbigstepforward"
actionTxt Chapterorbigstepback    = "chapterorbigstepback"
actionTxt Osd                     = "osd"
actionTxt Showsubtitles           = "showsubtitles"
actionTxt Nextsubtitle            = "nextsubtitle"
actionTxt Cyclesubtitle           = "cyclesubtitle"
actionTxt Playerdebug             = "playerdebug"
actionTxt Codecinfo               = "codecinfo"
actionTxt Playerprocessinfo       = "playerprocessinfo"
actionTxt Nextpicture             = "nextpicture"
actionTxt Previouspicture         = "previouspicture"
actionTxt Zoomout                 = "zoomout"
actionTxt Zoomin                  = "zoomin"
actionTxt Playlist                = "playlist"
actionTxt Queue                   = "queue"
actionTxt Zoomnormal              = "zoomnormal"
actionTxt Zoomlevel1              = "zoomlevel1"
actionTxt Zoomlevel2              = "zoomlevel2"
actionTxt Zoomlevel3              = "zoomlevel3"
actionTxt Zoomlevel4              = "zoomlevel4"
actionTxt Zoomlevel5              = "zoomlevel5"
actionTxt Zoomlevel6              = "zoomlevel6"
actionTxt Zoomlevel7              = "zoomlevel7"
actionTxt Zoomlevel8              = "zoomlevel8"
actionTxt Zoomlevel9              = "zoomlevel9"
actionTxt Nextcalibration         = "nextcalibration"
actionTxt Resetcalibration        = "resetcalibration"
actionTxt Analogmove              = "analogmove"
actionTxt Analogmovex             = "analogmovex"
actionTxt Analogmovey             = "analogmovey"
actionTxt Rotate                  = "rotate"
actionTxt Rotateccw               = "rotateccw"
actionTxt Close                   = "close"
actionTxt Subtitledelayminus      = "subtitledelayminus"
actionTxt Subtitledelay           = "subtitledelay"
actionTxt Subtitledelayplus       = "subtitledelayplus"
actionTxt Audiodelayminus         = "audiodelayminus"
actionTxt Audiodelay              = "audiodelay"
actionTxt Audiodelayplus          = "audiodelayplus"
actionTxt Subtitleshiftup         = "subtitleshiftup"
actionTxt Subtitleshiftdown       = "subtitleshiftdown"
actionTxt Subtitlealign           = "subtitlealign"
actionTxt Audionextlanguage       = "audionextlanguage"
actionTxt Verticalshiftup         = "verticalshiftup"
actionTxt Verticalshiftdown       = "verticalshiftdown"
actionTxt Nextresolution          = "nextresolution"
actionTxt Audiotoggledigital      = "audiotoggledigital"
actionTxt Number0                 = "number0"
actionTxt Number1                 = "number1"
actionTxt Number2                 = "number2"
actionTxt Number3                 = "number3"
actionTxt Number4                 = "number4"
actionTxt Number5                 = "number5"
actionTxt Number6                 = "number6"
actionTxt Number7                 = "number7"
actionTxt Number8                 = "number8"
actionTxt Number9                 = "number9"
actionTxt Smallstepback           = "smallstepback"
actionTxt Fastforward             = "fastforward"
actionTxt Rewind                  = "rewind"
actionTxt Play                    = "play"
actionTxt Playpause               = "playpause"
actionTxt Switchplayer            = "switchplayer"
actionTxt Delete                  = "delete"
actionTxt Copy                    = "copy"
actionTxt Move                    = "move"
actionTxt Screenshot              = "screenshot"
actionTxt Rename                  = "rename"
actionTxt Togglewatched           = "togglewatched"
actionTxt Scanitem                = "scanitem"
actionTxt Reloadkeymaps           = "reloadkeymaps"
actionTxt Volumeup                = "volumeup"
actionTxt Volumedown              = "volumedown"
actionTxt Mute                    = "mute"
actionTxt Backspace               = "backspace"
actionTxt Scrollup                = "scrollup"
actionTxt Scrolldown              = "scrolldown"
actionTxt Analogfastforward       = "analogfastforward"
actionTxt Analogrewind            = "analogrewind"
actionTxt Moveitemup              = "moveitemup"
actionTxt Moveitemdown            = "moveitemdown"
actionTxt Contextmenu             = "contextmenu"
actionTxt Shift                   = "shift"
actionTxt Symbols                 = "symbols"
actionTxt Cursorleft              = "cursorleft"
actionTxt Cursorright             = "cursorright"
actionTxt Showtime                = "showtime"
actionTxt Analogseekforward       = "analogseekforward"
actionTxt Analogseekback          = "analogseekback"
actionTxt Showpreset              = "showpreset"
actionTxt Nextpreset              = "nextpreset"
actionTxt Previouspreset          = "previouspreset"
actionTxt Lockpreset              = "lockpreset"
actionTxt Randompreset            = "randompreset"
actionTxt Increasevisrating       = "increasevisrating"
actionTxt Decreasevisrating       = "decreasevisrating"
actionTxt Showvideomenu           = "showvideomenu"
actionTxt Enter                   = "enter"
actionTxt Increaserating          = "increaserating"
actionTxt Decreaserating          = "decreaserating"
actionTxt Setrating               = "setrating"
actionTxt Togglefullscreen        = "togglefullscreen"
actionTxt Nextscene               = "nextscene"
actionTxt Previousscene           = "previousscene"
actionTxt Nextletter              = "nextletter"
actionTxt Prevletter              = "prevletter"
actionTxt Jumpsms2                = "jumpsms2"
actionTxt Jumpsms3                = "jumpsms3"
actionTxt Jumpsms4                = "jumpsms4"
actionTxt Jumpsms5                = "jumpsms5"
actionTxt Jumpsms6                = "jumpsms6"
actionTxt Jumpsms7                = "jumpsms7"
actionTxt Jumpsms8                = "jumpsms8"
actionTxt Jumpsms9                = "jumpsms9"
actionTxt Filter                  = "filter"
actionTxt Filterclear             = "filterclear"
actionTxt Filtersms2              = "filtersms2"
actionTxt Filtersms3              = "filtersms3"
actionTxt Filtersms4              = "filtersms4"
actionTxt Filtersms5              = "filtersms5"
actionTxt Filtersms6              = "filtersms6"
actionTxt Filtersms7              = "filtersms7"
actionTxt Filtersms8              = "filtersms8"
actionTxt Filtersms9              = "filtersms9"
actionTxt Firstpage               = "firstpage"
actionTxt Lastpage                = "lastpage"
actionTxt Guiprofile              = "guiprofile"
actionTxt Red                     = "red"
actionTxt Green                   = "green"
actionTxt Yellow                  = "yellow"
actionTxt Blue                    = "blue"
actionTxt Increasepar             = "increasepar"
actionTxt Decreasepar             = "decreasepar"
actionTxt Volampup                = "volampup"
actionTxt Volampdown              = "volampdown"
actionTxt Volumeamplification     = "volumeamplification"
actionTxt Createbookmark          = "createbookmark"
actionTxt Createepisodebookmark   = "createepisodebookmark"
actionTxt Settingsreset           = "settingsreset"
actionTxt Settingslevelchange     = "settingslevelchange"
actionTxt Stereomode              = "stereomode"
actionTxt Nextstereomode          = "nextstereomode"
actionTxt Previousstereomode      = "previousstereomode"
actionTxt Togglestereomode        = "togglestereomode"
actionTxt Stereomodetomono        = "stereomodetomono"
actionTxt Channelup               = "channelup"
actionTxt Channeldown             = "channeldown"
actionTxt Previouschannelgroup    = "previouschannelgroup"
actionTxt Nextchannelgroup        = "nextchannelgroup"
actionTxt Playpvr                 = "playpvr"
actionTxt Playpvrtv               = "playpvrtv"
actionTxt Playpvrradio            = "playpvrradio"
actionTxt Record                  = "record"
actionTxt Togglecommskip          = "togglecommskip"
actionTxt Showtimerrule           = "showtimerrule"
actionTxt Leftclick               = "leftclick"
actionTxt Rightclick              = "rightclick"
actionTxt Middleclick             = "middleclick"
actionTxt Doubleclick             = "doubleclick"
actionTxt Longclick               = "longclick"
actionTxt Wheelup                 = "wheelup"
actionTxt Wheeldown               = "wheeldown"
actionTxt Mousedrag               = "mousedrag"
actionTxt Mousemove               = "mousemove"
actionTxt Tap                     = "tap"
actionTxt Longpress               = "longpress"
actionTxt Pangesture              = "pangesture"
actionTxt Zoomgesture             = "zoomgesture"
actionTxt Rotategesture           = "rotategesture"
actionTxt Swipeleft               = "swipeleft"
actionTxt Swiperight              = "swiperight"
actionTxt Swipeup                 = "swipeup"
actionTxt Swipedown               = "swipedown"
actionTxt Error                   = "error"
actionTxt Noop                    = "noop"

actionStr :: Action -> Value
actionStr action = String $ actionTxt action
