--------------------------------------------------------------------------------
---                                                                          ---
---                           Alans XMonad Config                            ---
---                           Based on DTs Config                            ---
---                                                                          ---
--------------------------------------------------------------------------------

-- Base
import           XMonad
import           System.IO (hPutStrLn)
import           System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import           XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import           XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import           XMonad.Actions.WindowGo (runOrRaise)
import           XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Data
import           Data.Char (isSpace)
import           Data.List
import           Data.Monoid
import           Data.Maybe (isJust)
import           Data.Tree
import qualified Data.Map as M

-- Hooks
import           XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import           XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import           XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import           XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import           XMonad.Hooks.SetWMName

-- Layouts
import           XMonad.Layout.GridVariants (Grid(Grid))
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spiral
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Reflect
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import           XMonad.Layout.Magnifier
import           XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed (renamed, Rename(Replace))
import           XMonad.Layout.Spacing
import           XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Prompt
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.Man
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell (shellPrompt)
import           XMonad.Prompt.Ssh
import           XMonad.Prompt.XMonad
import           Control.Arrow (first)

-- Utilities
import           XMonad.Util.EZConfig (additionalKeysP)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import           XMonad.Util.SpawnOnce

--------------------------------------------------------------------------------
---                                                                          ---
---                              Look And Feel                               ---
---                                                                          ---
--------------------------------------------------------------------------------

myFont :: String
myFont = "xft:Inconsolata Nerd Font:Regular:pixelsize=14:antialias=true:hinting=true"

myTerminal :: String
myTerminal = "alacritty"   -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColor :: String
myNormColor   = "#292d3e"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#bbc5ff"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod4Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--------------------------------------------------------------------------------
---                                                                          ---
---                                Autostart                                 ---
---                                                                          ---
--------------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
          spawnOnce "nitrogen --restore &"
          spawnOnce "compton &"
          spawnOnce "klipper &"
          spawnOnce "twmnd &"
          spawnOnce "/usr/bin/emacs --daemon &"
          spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x282A36 --height 19 &"
          setWMName "XMonad"

--------------------------------------------------------------------------------
---                                                                          ---
---                             XPrompt Settings                             ---
---                                                                          ---
--------------------------------------------------------------------------------

myXPConfig :: XPConfig
myXPConfig = def
      { font                = myFont
      , bgColor             = "#292d3e"
      , fgColor             = "#d0d0d0"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = myXPKeymap
      , position            = Top
--    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = isPrefixOf
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

-- The same config minus the autocomplete feature which is annoying on
-- certain Xprompts, like the search engine prompts.
myXPConfig' :: XPConfig
myXPConfig' = myXPConfig { autoComplete = Nothing }

-- A list of all of the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             ]

-- A list of my custom prompts
promptList' :: [(String, XPConfig -> String -> X (), String)]
promptList' = [ ("c", calcPrompt, "qalc")         -- requires qalculate-gtk
              ]
------------------------------------------------------------------------
-- Custom PROMPTS
------------------------------------------------------------------------
-- calcPrompt requires a cli calculator called qalcualte-gtk.
-- You could use this as a template for other custom prompts that
-- use command line programs that return a single line of output.
calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
    where
        trim  = f . f
            where f = reverse . dropWhile isSpace

--------------------------------------------------------------------------------
---                                                                          ---
---                 XPrompt keymap (emacs-like key bindings)                 ---
---                                                                          ---
--------------------------------------------------------------------------------

myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
myXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line fowards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

--------------------------------------------------------------------------------
---                                                                          ---
---                              Search Engines                              ---
---                                                                          ---
--------------------------------------------------------------------------------

-- Xmonad has several search engines available to use located in
-- XMonad.Actions.Search. Additionally, you can add other search engines
-- such as those listed below.
archwiki, reddit :: S.SearchEngine

archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("g", S.google)
             , ("i", S.images)
             , ("r", reddit)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("b", S.wayback)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]

--------------------------------------------------------------------------------
---                                                                          ---
---                               Keybindings                                ---
---                                                                          ---
--------------------------------------------------------------------------------

-- I am using the Xmonad.Util.EZConfig module which allows keybindings
-- to be written in simpler, emacs-like format.
myKeys :: [(String, X ())]
myKeys =
    -- Lock screen
        [ ("M-S-l", spawn "slock")
    -- Xmonad
        , ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad

    -- Open my preferred terminal
        , ("M-<Return>", spawn (myTerminal))

    -- Run Prompt
        , ("M-S-<Return>", shellPrompt myXPConfig)   -- Shell Prompt

    -- Windows
        , ("M-S-c", kill1)                           -- Kill the currently focused client
        , ("M-S-a", killAll)                         -- Kill all windows on current workspace

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats"))       -- Toggles my 'floats' layout
        , ("M-<Delete>", withFocused $ windows . W.sink) -- Push floating window back to tile
        , ("M-S-<Delete>", sinkAll)                      -- Push ALL floating windows to tile

    -- Windows navigation
        , ("M-m", windows W.focusMaster)     -- Move focus to the master window
        , ("M-j", windows W.focusDown)       -- Move focus to the next window
        , ("M-k", windows W.focusUp)         -- Move focus to the prev window
        --, ("M-S-m", windows W.swapMaster)    -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)      -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)        -- Swap focused window with prev window
        , ("M-<Backspace>", promote)         -- Moves focused window to master, others maintain order
        , ("M1-S-<Tab>", rotSlavesDown)      -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)         -- Rotate all the windows in the current stack
        --, ("M-S-s", windows copyToAll)
        , ("M-C-s", killAllOtherCopies)

        -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-<Space>", sendMessage ToggleStruts)         -- Toggles struts
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)      -- Toggles noborder
        , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in master pane
        , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in master pane
        , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows
        , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows

        , ("M-C-l", sendMessage Shrink)       -- Shrink horiz window width
        , ("M-C-h", sendMessage Expand)       -- Expand horiz window width
        , ("M-C-j", sendMessage MirrorShrink) -- Shrink vert window width
        , ("M-C-k", sendMessage MirrorExpand) -- Exoand vert window width

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- Scratchpads
        , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")

    -- Emacs
        , ("M-e e", spawn "emacsclient -c -a ''")                           -- start emacs
        , ("M-e a", spawn "emacsclient -c -a '' --eval '(emms)'")           -- emms emacs audio player
        , ("M-e b", spawn "emacsclient -c -a '' --eval '(ibuffer)'")        -- list emacs buffers
        , ("M-e d", spawn "emacsclient -c -a '' --eval '(dired nil)'")      -- dired emacs file manager
        , ("M-e m", spawn "emacsclient -c -a '' --eval '(mu4e)'")           -- mu4e emacs email client
        , ("M-e n", spawn "emacsclient -c -a '' --eval '(elfeed)'")         -- elfeed emacs rss client
        , ("M-e s", spawn "emacsclient -c -a '' --eval '(eshell)'")         -- eshell within emacs
        , ("M-e t", spawn "emacsclient -c -a '' --eval '(+vterm/here nil)'")         -- eshell within emacs

    -- Multimedia Keys
        , ("<XF86AudioPlay>", spawn "cmus toggle")
        , ("<XF86AudioPrev>", spawn "cmus prev")
        , ("<XF86AudioNext>", spawn "cmus next")
        -- , ("<XF86AudioMute>",   spawn "amixer set Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.google.com/"])
        , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
        , ("<XF86Eject>", spawn "toggleeject")
        , ("<Print>", spawn "scrotd 0")
        ]
        -- Appending search engines to keybindings list
        ++ [("M-s " ++ k, S.promptSearch myXPConfig' f) | (k,f) <- searchList ]
        ++ [("M-S-s " ++ k, S.selectSearchBrowser "firefox" f) | (k,f) <- searchList ]
        ++ [("M-p " ++ k, f myXPConfig') | (k,f) <- promptList ]
        ++ [("M-p " ++ k, f myXPConfig' g) | (k,f,g) <- promptList' ]
        -- Appending named scratchpads to keybindings list
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

--------------------------------------------------------------------------------
---                                                                          ---
---                                Workspaces                                ---
---                                                                          ---
--------------------------------------------------------------------------------

myWorkspaces :: Forest String
myWorkspaces = [ Node "\xf488 "  [] -- a workspace for your browser
               , Node "\xf489 " [] -- for everyday activity's
               , Node "\xf108  3"  [] -- for everyday activity's
               , Node "\xf108  4"  [] -- for everyday activity's
               , Node "\xf108  5"  [] -- for everyday activity's
               , Node "\xf108  6"  [] -- for everyday activity's
               , Node "\xf108  7"  [] -- for everyday activity's
               , Node "\xf108  8"  [] -- for everyday activity's
               , Node "\xf6ef  9"  [] -- for everyday activity's
               ]

--------------------------------------------------------------------------------
---                                                                          ---
---                                  Hooks                                   ---
---                                                                          ---
--------------------------------------------------------------------------------

-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace. You
-- need the className or title of the program. Use xprop to get this info.

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "firefox"   --> doShift ( "\xf488 ")
     , className =? "vlc"       --> doShift ( "\xf488 ")
     -- Float Firefox Dialog
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
     ] <+> namedScratchpadManageHook myScratchPads

--------------------------------------------------------------------------------
---                                                                          ---
---                                 Layouts                                  ---
---                                                                          ---
--------------------------------------------------------------------------------

-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 4
           $ reflectHoriz $ ResizableTall 1 (3/100) (2/3) []
magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = myFont
                      , activeColor         = "#292d3e"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#292d3e"
                      , inactiveBorderColor = "#292d3e"
                      , activeTextColor     = "#ffffff"
                      , inactiveTextColor   = "#d0d0d0"
                      }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     tall
                                 ||| magnify
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| grid
                                 ||| noBorders tabs

--------------------------------------------------------------------------------
---                                                                          ---
---                               Scratchpads                                ---
---                                                                          ---
--------------------------------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 1
                 w = 1
                 t = 0
                 l = 0

--------------------------------------------------------------------------------
---                                                                          ---
---                                   Main                                   ---
---                                                                          ---
--------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc0 <- spawnPipe "xmobar -x 0 /home/alan/.config/xmobar/xmobarrc0.hs"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        -- Run xmonad commands from command line with "xmonadctl command". Commands include:
        -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
        -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
        -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
        , handleEventHook    = docksEventHook
        , modMask            = altMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = TS.toWorkspaces myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ xmobarPP
                       { ppOutput = \x -> hPutStrLn xmproc0 x
                       , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                       , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                       , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                       , ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
                       , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60     -- Title of active window in xmobar
                       , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
                       , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                       , ppExtras  = [windowCount]                           -- # of windows current workspace
                       , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                       }
        } `additionalKeysP` myKeys
