-- http://projects.haskell.org/xmobar/
-- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
-- you can find weather location codes here: http://weather.noaa.gov/index.html

Config { font    = "xft:Inconsolata Nerd Font:Regular:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Inconsolata Nerd Font:Regular:pixelsize=14" ]
       , bgColor = "#282A36"
       , fgColor = "#B45BCF"
       , position = Top
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "/home/alan/.xmonad/xpm/"  -- default: "."
       , commands = [ 
                      -- Time and date
                      Run Date "\xf133  %b %d %Y (%H:%M)" "date" 50
                      -- Batery
                    , Run BatteryP ["BAT0"] ["-t", "\xf240  <left>%"] 60
                      -- Script that dynamically adjusts xmobar padding depending on number of trayer icons.
                    , Run Com "bash" ["/home/alan/.config/xmobar/trayer-padding-icon.sh"] "trayerpad" 10
                      -- Prints out the left side items such as workspaces, layout, etc.
                      -- The workspaces are 'clickable' in my configs.
                    , Run UnsafeStdinReader
                    , Run Com "bash" ["/home/alan/.config/xmobar/volume.sh"] "volume" 1
                    , Run Wireless "wlan0" [ "-t", "\xf1eb  <essid>" ] 10
                    -- network activity monitor (dynamic interface resolution)
                    , Run DynNetwork [ "--template" , "\xf700  <dev>"
                             , "--Low"      , "1000"       -- units: B/s
                             , "--High"     , "5000"       -- units: B/s
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell_20.xpm/> \
                    \<fc=#666666>|</fc>\
                    \ %UnsafeStdinReader% }{ \
                    \<fc=#c3e88d> %volume% </fc>\
                    \<fc=#666666>|</fc>\
                    \<fc=#c3e88d> %dynnetwork% </fc>\
                    \<fc=#666666>|</fc>\
                    \<fc=#c3e88d> %wlan0wi% </fc>\
                    \<fc=#666666>|</fc>\
                    \<fc=#c3e88d> %battery% </fc>\
                    \<fc=#666666>|</fc>\
                    \<fc=#8BE9FD> %date% </fc>\
                    \%trayerpad%"
       }
