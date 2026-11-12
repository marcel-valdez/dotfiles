-- =============================================================================
-- ~/.hammerspoon/init.lua: Dual-OS Window Manager & X11/XFCE4 Parity Engine
-- =============================================================================
-- Replicates Marcel's MTV workstation and WFH laptop X11/XFCE4 window management
-- workflows natively on macOS without hijacking standard macOS Cmd+Z/C/V/X/K/N:
--   1. Stateful 6-tier fractional window cycling (x-window-move port):
--      {1.0, 0.75, 0.666, 0.5, 0.334, 0.25} on WM_MOD (Ctrl+Cmd) + Arrows/K.
--   2. Cross-monitor window movement & dual-screen spanning (both-screens) on
--      both WFH (<Ctrl><Cmd>Page_Up) and MTV (<Alt><Cmd>Left/Right/Page_Up).
--   3. 6-Space workspace navigation (<Ctrl><Alt>1..6, <Ctrl><Alt>Left/Right; left to
--      macOS when its own shortcut uses the same keys) and window migration that
--      follows the window (<Ctrl><Shift><Alt>Left/Right and <Ctrl><Shift><Alt>1..6).
--   4. Sticky Window toggle (<Alt>F6) + automatic Sticky Kitty watcher via
--      hs.spaces.watcher (bypassing macOS SIP restrictions on third-party apps).
--   5. Borderless Alt+Left-Drag window move & Alt+Right-Drag window resize
--      (replicating xfwm4 easy_click=Alt).
--   6. <Alt>Tab MRU fuzzy window switcher (hs.chooser) with live 10px 85%-opaque
--      blue highlight border (hs.canvas, replicating draw_border_around_window.py).
--   7. <WM_MOD>z Zen Mode toggle & <WM_MOD>n terminal-scoped Notes launcher.
--   8. Re-sync of the CR240DM on the Elgato dock's DisplayPort after the displays
--      wake (it can come back blank until its refresh rate is switched and
--      switched back); after an hour-plus display sleep, contrast 50 and a
--      10-minute warm-up at brightness 25, then 50 (its backlight gives out above
--      25 while cold); <WM_MOD>6/7 and <WM_MOD>F6/F7 step the left monitor's
--      brightness and contrast by 25.
-- =============================================================================

-- Live Hammerspoon CLI (`/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -c '<lua>'`)
-- and `open -g hammerspoon://reload` for interactive debugging and config reloads.
require("hs.ipc")
hs.urlevent.bind("reload", function() hs.reload() end)
pcall(hs.autoLaunch, true)

-- Root table in _G so Lua 5.4 garbage collection never collects hotkeys,
-- CGEventTaps, space watchers, window filters, choosers, or pending timers
-- a few seconds after init.lua finishes executing.
_G.HS_KEEPALIVE = {
  hotkeys = {},
  watchers = {},
  taps = {},
  timers = {},
  filters = {},
  choosers = {},
}

local function keepHotkey(mods, key, fn)
  local hk = hs.hotkey.bind(mods, key, fn)
  table.insert(_G.HS_KEEPALIVE.hotkeys, hk)
  return hk
end

hs.window.animationDuration = 0

local WM_MOD       = {"ctrl", "cmd"}
local WM_MOD_SHIFT = {"ctrl", "shift", "cmd"}
local ALT_SUPER    = {"alt", "cmd"}

-- 6-tier fractional sizing sequence matching ~/bin/x-window-move
local CYCLE_RATIOS = {1.0, 0.75, 0.666, 0.5, 0.334, 0.25}
local EPSILON      = 0.03

-- Track per-window previous frames for unmaximize / both-screens restore
local savedWindowFrames = {}

-- -----------------------------------------------------------------------------
-- 1. Fractional 6-Tier Window Cycling (x-window-move port)
-- -----------------------------------------------------------------------------
local function nextCycleRatio(currentRatio, reverseOrder)
  local matchedIdx = nil
  for i, ratio in ipairs(CYCLE_RATIOS) do
    if math.abs(currentRatio - ratio) <= EPSILON then
      matchedIdx = i
      break
    end
  end

  if not matchedIdx then
    return reverseOrder and CYCLE_RATIOS[#CYCLE_RATIOS] or CYCLE_RATIOS[1]
  end

  if reverseOrder then
    local prevIdx = matchedIdx - 1
    if prevIdx < 1 then prevIdx = #CYCLE_RATIOS end
    return CYCLE_RATIOS[prevIdx]
  else
    local nextIdx = matchedIdx + 1
    if nextIdx > #CYCLE_RATIOS then nextIdx = 1 end
    return CYCLE_RATIOS[nextIdx]
  end
end

local function cycleWindowSnap(direction, reverseOrder)
  local win = hs.window.focusedWindow()
  if not win then return end

  local screenFrame = win:screen():frame()
  local winFrame    = win:frame()

  if direction == "left" then
    local alignedLeft = math.abs(winFrame.x - screenFrame.x) <= 20
    local currentRatio = alignedLeft and (winFrame.w / screenFrame.w) or 0
    local targetRatio = nextCycleRatio(currentRatio, reverseOrder)
    win:setFrame({
      x = screenFrame.x,
      y = screenFrame.y,
      w = math.floor(screenFrame.w * targetRatio),
      h = screenFrame.h,
    })
  elseif direction == "right" then
    local rightEdgeDist = math.abs((winFrame.x + winFrame.w) - (screenFrame.x + screenFrame.w))
    local currentRatio = (rightEdgeDist <= 20) and (winFrame.w / screenFrame.w) or 0
    local targetRatio = nextCycleRatio(currentRatio, reverseOrder)
    local targetWidth = math.floor(screenFrame.w * targetRatio)
    win:setFrame({
      x = screenFrame.x + screenFrame.w - targetWidth,
      y = screenFrame.y,
      w = targetWidth,
      h = screenFrame.h,
    })
  elseif direction == "up" then
    local alignedTop = math.abs(winFrame.y - screenFrame.y) <= 20
    local currentRatio = alignedTop and (winFrame.h / screenFrame.h) or 0
    local targetRatio = nextCycleRatio(currentRatio, reverseOrder)
    win:setFrame({
      x = winFrame.x,
      y = screenFrame.y,
      w = winFrame.w,
      h = math.floor(screenFrame.h * targetRatio),
    })
  elseif direction == "down" then
    local bottomEdgeDist = math.abs((winFrame.y + winFrame.h) - (screenFrame.y + screenFrame.h))
    local currentRatio = (bottomEdgeDist <= 20) and (winFrame.h / screenFrame.h) or 0
    local targetRatio = nextCycleRatio(currentRatio, reverseOrder)
    local targetHeight = math.floor(screenFrame.h * targetRatio)
    win:setFrame({
      x = winFrame.x,
      y = screenFrame.y + screenFrame.h - targetHeight,
      w = winFrame.w,
      h = targetHeight,
    })
  elseif direction == "center" then
    local centerX = screenFrame.x + (screenFrame.w - winFrame.w) / 2
    local alignedCenter = math.abs(winFrame.x - centerX) <= 25
    local currentRatio = alignedCenter and (winFrame.w / screenFrame.w) or 0
    local targetRatio = nextCycleRatio(currentRatio, reverseOrder)
    local targetWidth = math.floor(screenFrame.w * targetRatio)
    win:setFrame({
      x = math.floor(screenFrame.x + (screenFrame.w - targetWidth) / 2),
      y = screenFrame.y,
      w = targetWidth,
      h = screenFrame.h,
    })
  end
end

-- Bind WM_MOD (Ctrl+Cmd) + Arrows / K for 6-tier window cycling
keepHotkey(WM_MOD,       "Left",  function() cycleWindowSnap("left",   false) end)
keepHotkey(WM_MOD_SHIFT, "Left",  function() cycleWindowSnap("left",   true)  end)
keepHotkey(WM_MOD,       "Right", function() cycleWindowSnap("right",  false) end)
keepHotkey(WM_MOD_SHIFT, "Right", function() cycleWindowSnap("right",  true)  end)
keepHotkey(WM_MOD,       "Up",    function() cycleWindowSnap("up",     false) end)
keepHotkey(WM_MOD_SHIFT, "Up",    function() cycleWindowSnap("up",     true)  end)
keepHotkey(WM_MOD,       "Down",  function() cycleWindowSnap("down",   false) end)
keepHotkey(WM_MOD_SHIFT, "Down",  function() cycleWindowSnap("down",   true)  end)
keepHotkey(WM_MOD,       "k",     function() cycleWindowSnap("center", false) end)
keepHotkey(WM_MOD_SHIFT, "k",     function() cycleWindowSnap("center", true)  end)

-- -----------------------------------------------------------------------------
-- 2. Cross-Monitor Movement, Maximize, & Dual-Monitor Span (both-screens)
-- -----------------------------------------------------------------------------
local function moveWindowToScreenInDirection(dir)
  local win = hs.window.focusedWindow()
  if not win then return end
  local targetScreen = (dir == "west") and win:screen():toWest() or win:screen():toEast()
  if not targetScreen then
    targetScreen = (dir == "west") and win:screen():previous() or win:screen():next()
  end
  if targetScreen and targetScreen ~= win:screen() then
    win:moveToScreen(targetScreen, false, true)
  end
end

local function toggleSpanBothScreens()
  local win = hs.window.focusedWindow()
  if not win then return end
  local winId = win:id()
  local screens = hs.screen.allScreens()
  if #screens < 2 then
    win:maximize()
    return
  end

  -- Compute bounding union box across all connected displays
  local minX, minY = math.huge, math.huge
  local maxX, maxY = -math.huge, -math.huge
  for _, scr in ipairs(screens) do
    local f = scr:frame()
    if f.x < minX then minX = f.x end
    if f.y < minY then minY = f.y end
    if (f.x + f.w) > maxX then maxX = f.x + f.w end
    if (f.y + f.h) > maxY then maxY = f.y + f.h end
  end
  local unionFrame = {x = minX, y = minY, w = maxX - minX, h = maxY - minY}
  local cur = win:frame()

  if math.abs(cur.w - unionFrame.w) <= 40 and savedWindowFrames[winId] then
    win:setFrame(savedWindowFrames[winId])
    savedWindowFrames[winId] = nil
  else
    savedWindowFrames[winId] = cur
    win:setFrame(unionFrame)
  end
end

-- MTV & WFH cross-monitor bindings (<Alt><Cmd>Left/Right/Page_Up and <Ctrl><Cmd>Page_Up/Page_Down)
keepHotkey(ALT_SUPER,    "Left",     function() moveWindowToScreenInDirection("west") end)
keepHotkey(ALT_SUPER,    "Right",    function() moveWindowToScreenInDirection("east") end)
keepHotkey(ALT_SUPER,    "pageup",   toggleSpanBothScreens)
keepHotkey(WM_MOD,       "pageup",   toggleSpanBothScreens)
keepHotkey(WM_MOD_SHIFT, "pageup",   function()
  local win = hs.window.focusedWindow()
  if win then win:maximize() end
end)
keepHotkey(WM_MOD,       "pagedown", function()
  local win = hs.window.focusedWindow()
  if win then win:minimize() end
end)

-- -----------------------------------------------------------------------------
-- 3. 6-Space Workspace Switching (<Ctrl><Alt>1..6 & Left/Right) & Window-to-Space
--    Moves that follow the window (<Ctrl><Shift><Alt>Left/Right & <Ctrl><Shift><Alt>1..6)
-- -----------------------------------------------------------------------------
-- macOS 14.5+ (15 Sequoia, 26 Tahoe) NOPs CGSMoveWindowsToManagedSpace, so
-- hs.spaces.moveWindowToSpace() returns true while the window stays put. Windows
-- are therefore moved the way a human moves them (verified live on macOS 26.7):
--   A. Titled windows: hold the window by a draggable titlebar point (synthetic
--      leftMouseDown + leftMouseDragged) and post the user's *own* macOS
--      "Move left/right a space" (symbolic hotkeys 79/81) or "Switch to Desktop N"
--      (118..133) shortcut, read live from com.apple.symbolichotkeys (e.g.
--      Ctrl+Alt+Arrow on the gMac rather than the stock Ctrl+Arrow; an entry
--      without key parameters, such as {enabled = 1}, means the stock binding;
--      arrow-key shortcuts must carry the fn flag). WindowServer carries the held
--      window to the new Space. hs.spaces.gotoSpace() switches via a Mission
--      Control AXPress and does NOT carry a held window, so it cannot be used for this.
--   B. Borderless windows (kitty hide_window_decorations yes), titlebars with no
--      draggable spot, or no enabled Spaces shortcut: open Mission Control, wait
--      for the thumbnails to settle, press the window's thumbnail and lift it (the
--      Spaces bar expands only once a drag is in progress), drop it onto the
--      *expanded* target Space thumbnail, then, once Mission Control has settled,
--      AXPress that Space to follow it (if Mission Control ignores the press, close
--      it and switch Spaces directly).
-- Both paths first wait for the physically held hotkey modifiers to be released
-- (taps made meanwhile accumulate: hold Ctrl+Alt+Shift and tap Right twice to move
-- two Spaces), keep strong references to every pending hs.timer (unreferenced
-- hs.timer.doAfter timers are garbage-collected and silently never fire), and
-- always release the synthetic mouse button and restore the cursor in finishMove().
local altDragTap = nil            -- assigned in Section 5
local stickyWindows = {}          -- Section 4: winId -> true (pinned) | false (explicitly unpinned)
local onWindowMovedToSpace = nil  -- assigned in Section 6: refreshes the <Alt>Tab window tracker
local spaceMoveLog = hs.logger.new("spacemove", "info")

local evTypes = hs.eventtap.event.types
local evProps = hs.eventtap.event.properties
-- Stamped (eventSourceUserData) on keystrokes Hammerspoon posts itself, so the
-- workspace-shortcut key tap ignores them instead of treating them as user presses.
local SYNTHETIC_EVENT_TAG = 0x48534D56  -- "HSMV"
local nextMouseEventNumber = 32000

local function allocMouseEventNumber()
  nextMouseEventNumber = nextMouseEventNumber + 1
  if nextMouseEventNumber >= 0x7fffffff then
    nextMouseEventNumber = 32001
  end
  return nextMouseEventNumber
end

local function roundPt(pt)
  return { x = math.floor(pt.x + 0.5), y = math.floor(pt.y + 0.5) }
end

local function indexOf(list, value)
  for i, v in ipairs(list or {}) do
    if v == value then return i end
  end
  return nil
end

local function postRawMouseMove(pt)
  hs.eventtap.event.newMouseEvent(evTypes.mouseMoved, roundPt(pt)):rawFlags(0):post()
end

-- Integer event fields must receive integral Lua numbers: hs.geometry centers are
-- fractional (e.g. 512.5) and setProperty() then raises "number has no integer
-- representation", which used to abort the Mission Control drag mid-way with the
-- synthetic left mouse button still held down.
local function postRawMouseButton(evType, pt, evNum, dx, dy)
  local ev = hs.eventtap.event.newMouseEvent(evType, roundPt(pt))
    :rawFlags(0)
    :setProperty(evProps.mouseEventNumber, math.floor(evNum))
    :setProperty(evProps.mouseEventClickState, 1)
    :setProperty(evProps.mouseEventDeltaX, math.floor((dx or 0) + 0.5))
    :setProperty(evProps.mouseEventDeltaY, math.floor((dy or 0) + 0.5))
  ev:post()
end

local function getRawSpacesForScreen(scr)
  scr = scr or hs.screen.mainScreen()
  local allSpaces = hs.spaces.allSpaces()
  if not allSpaces then return {} end
  if scr then
    local byScreen = allSpaces[scr:getUUID()]
    if byScreen and #byScreen > 0 then return byScreen end
  end
  local primary = hs.screen.primaryScreen()
  if primary then
    local byPrimary = allSpaces[primary:getUUID()]
    if byPrimary and #byPrimary > 0 then return byPrimary end
  end
  for _, list in pairs(allSpaces) do
    if type(list) == "table" and #list > 0 then return list end
  end
  return {}
end

local function getOrderedUserSpacesForCurrentScreen(scr)
  local rawSpaces = getRawSpacesForScreen(scr)
  local userSpaces = {}
  for _, spId in ipairs(rawSpaces) do
    if hs.spaces.spaceType(spId) == "user" then
      table.insert(userSpaces, spId)
    end
  end
  return userSpaces
end

-- Adds Spaces until scr has minCount user Spaces. hs.spaces.addSpaceToScreen()
-- presses Mission Control's "mc.spaces.add" button (closeMC=false keeps Mission
-- Control open between presses); Space creation is asynchronous, so poll
-- allSpaces() for each new Space and close Mission Control afterwards.
local function ensureUserSpacesCount(scr, minCount)
  scr = scr or hs.screen.mainScreen()
  if not scr then return {} end
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  local openedMissionControl = false
  while #spaces < minCount do
    openedMissionControl = true
    local ok, added = pcall(hs.spaces.addSpaceToScreen, scr, false)
    if not ok or not added then
      spaceMoveLog.w("hs.spaces.addSpaceToScreen failed: " .. tostring(added))
      break
    end
    local updated = spaces
    local deadline = hs.timer.secondsSinceEpoch() + 1.5
    while #updated <= #spaces and hs.timer.secondsSinceEpoch() < deadline do
      hs.timer.usleep(50000)
      updated = getOrderedUserSpacesForCurrentScreen(scr)
    end
    if #updated <= #spaces then break end
    spaces = updated
  end
  if openedMissionControl then
    pcall(hs.spaces.closeMissionControl)
  end
  return spaces
end

-- Fading "Desktop N" banner (with position dots) shown only for desktop switches
-- made through these Hammerspoon shortcuts -- OS-level switches are never hooked.
-- Each banner is a fresh canvas that joins all Spaces and is stationary, so it is
-- visible at once and stays put through the Space-switch animation; a new banner
-- replaces the previous one instead of stacking.
local BANNER_VISIBLE_SEC = 0.9
local BANNER_FADE_SEC = 0.35
local workspaceBanner = nil
local workspaceBannerTimer = nil

local function showWorkspaceBanner(scr, idx, total, detail)
  scr = scr or hs.screen.mainScreen()
  if not scr or not idx then return end
  total = math.max(total or idx, idx)
  local full = scr:fullFrame()
  local spacing = 22
  local w = math.max(230, (total - 1) * spacing + 90)
  local h = detail and 118 or 96
  local elements = {
    { type = "rectangle", action = "fill", fillColor = { white = 0.08, alpha = 0.82 },
      roundedRectRadii = { xRadius = 16, yRadius = 16 } },
    { type = "text", text = string.format("Desktop %d", idx), textFont = ".AppleSystemUIFontBold",
      textSize = 30, textColor = { white = 1, alpha = 1 }, textAlignment = "center",
      frame = { x = 0, y = 12, w = w, h = 40 } },
  }
  local x0 = (w - (total - 1) * spacing) / 2
  for i = 1, total do
    table.insert(elements, {
      type = "circle", center = { x = x0 + (i - 1) * spacing, y = 66 }, radius = 5,
      action = "strokeAndFill", strokeWidth = 1.5, strokeColor = { white = 1, alpha = 0.9 },
      fillColor = { white = 1, alpha = (i == idx) and 1 or 0.12 },
    })
  end
  if detail then
    table.insert(elements, {
      type = "text", text = detail, textSize = 13, textColor = { white = 1, alpha = 0.85 },
      textAlignment = "center", frame = { x = 8, y = 86, w = w - 16, h = 22 },
    })
  end

  if workspaceBannerTimer then workspaceBannerTimer:stop() end
  if workspaceBanner then workspaceBanner:delete(0.1) end
  local banner = hs.canvas.new({ x = full.x + (full.w - w) / 2, y = full.y + full.h * 0.36 - h / 2, w = w, h = h })
  banner:level(hs.canvas.windowLevels.overlay)
  banner:behaviorAsLabels({ "canJoinAllSpaces", "stationary" })
  banner:clickActivating(false)
  banner:replaceElements(elements)
  banner:show(0.08)
  workspaceBanner = banner
  workspaceBannerTimer = hs.timer.doAfter(BANNER_VISIBLE_SEC, function()
    workspaceBannerTimer = nil
    if workspaceBanner == banner then workspaceBanner = nil end
    banner:delete(BANNER_FADE_SEC)
  end)
end

local function showBannerForSpace(scr, spaceId, detail)
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  local idx = indexOf(spaces, spaceId)
  if idx then showWorkspaceBanner(scr, idx, #spaces, detail) end
end

-- Desktop switches complete asynchronously (macOS's own shortcuts in ~0.3s,
-- gotoSpace() via Mission Control in ~1s), so the banner waits for the focused
-- Space to actually change: every change within the armed window gets a banner
-- (holding or repeating a shortcut moves several Desktops).
local switchBannerArm = { timer = nil, deadline = 0, lastSpace = nil, scr = nil }

local function armSwitchBanner(scr, windowSec)
  local arm = switchBannerArm
  arm.scr = scr or hs.screen.mainScreen()
  arm.deadline = math.max(arm.deadline, hs.timer.secondsSinceEpoch() + (windowSec or 1.6))
  if arm.timer then return end
  arm.lastSpace = hs.spaces.focusedSpace()
  arm.timer = hs.timer.doEvery(0.03, function()
    local fs = hs.spaces.focusedSpace()
    if fs and fs ~= arm.lastSpace then
      arm.lastSpace = fs
      showBannerForSpace(arm.scr, fs)
    end
    if hs.timer.secondsSinceEpoch() >= arm.deadline then
      arm.timer:stop()
      arm.timer = nil
    end
  end)
end

-- macOS Spaces shortcuts (System Settings > Keyboard > Keyboard Shortcuts >
-- Mission Control) live in com.apple.symbolichotkeys as {ascii, keycode, flags},
-- where flags use the CGEventFlags bits (0x20000 shift, 0x40000 ctrl, 0x80000 alt,
-- 0x100000 cmd, 0x800000 fn).
local SYMBOLIC_MOVE_LEFT_SPACE     = 79
local SYMBOLIC_MOVE_RIGHT_SPACE    = 81
local SYMBOLIC_SWITCH_TO_DESKTOP_1 = 118  -- 118..133 = "Switch to Desktop 1..16"
local SYMBOLIC_HOTKEY_DEFAULTS = {        -- macOS defaults when the plist has no entry
  [SYMBOLIC_MOVE_LEFT_SPACE]  = { keycode = 123, flags = 0x840000 },  -- Ctrl+Left
  [SYMBOLIC_MOVE_RIGHT_SPACE] = { keycode = 124, flags = 0x840000 },  -- Ctrl+Right
}
local HOTKEY_MOD_FLAGS = { shift = 0x20000, ctrl = 0x40000, alt = 0x80000, cmd = 0x100000 }
local MODIFIER_FLAGS_MASK = 0x1E0000      -- shift | ctrl | alt | cmd
local SYMBOLIC_HOTKEYS_PLIST = os.getenv("HOME") .. "/Library/Preferences/com.apple.symbolichotkeys.plist"
local symbolicHotkeysCache, symbolicHotkeysReadAt, symbolicHotkeysMtime = nil, 0, nil

local function readSymbolicHotkeys()
  local now = hs.timer.secondsSinceEpoch()
  local mtime = hs.fs.attributes(SYMBOLIC_HOTKEYS_PLIST, "modification")
  -- Cached until the plist changes on disk (System Settings edits are flushed there
  -- within seconds), for at most 5 minutes, and invalidated whenever a posted
  -- shortcut has no effect
  if symbolicHotkeysCache and mtime == symbolicHotkeysMtime and (now - symbolicHotkeysReadAt) < 300 then
    return symbolicHotkeysCache
  end
  local data = nil
  -- `defaults export` goes through cfprefsd, so recent Settings changes are seen
  local xml = hs.execute("/usr/bin/defaults export com.apple.symbolichotkeys - 2>/dev/null")
  if xml and #xml > 0 then
    local ok, parsed = pcall(hs.plist.readString, xml)
    if ok and type(parsed) == "table" then data = parsed end
  end
  if not data then
    data = hs.plist.read(SYMBOLIC_HOTKEYS_PLIST)
  end
  symbolicHotkeysCache = (type(data) == "table" and data.AppleSymbolicHotKeys) or {}
  symbolicHotkeysReadAt = now
  symbolicHotkeysMtime = mtime
  return symbolicHotkeysCache
end

-- Returns { keycode = n, flags = n } for a com.apple.symbolichotkeys entry, or nil
-- when it is disabled. An entry without key parameters (e.g. {enabled = 1}, as
-- on a fresh gMac) uses the macOS default binding, just like a missing entry:
-- WindowServer then reports Ctrl+Left/Right for 79/81.
local function symbolicHotkeyFromEntry(id, entry)
  if entry == nil then
    return SYMBOLIC_HOTKEY_DEFAULTS[id]
  end
  if entry.enabled == false or entry.enabled == 0 then return nil end
  local params = type(entry.value) == "table" and entry.value.parameters or nil
  if type(params) ~= "table" then
    return SYMBOLIC_HOTKEY_DEFAULTS[id]
  end
  local keycode = tonumber(params[2])
  if not keycode or keycode == 65535 then return nil end
  return { keycode = math.floor(keycode), flags = math.floor(tonumber(params[3]) or 0) }
end

-- Returns { keycode = n, flags = n } for an enabled symbolic hotkey id, or nil.
local function getSymbolicHotkey(id)
  return symbolicHotkeyFromEntry(id, readSymbolicHotkeys()[tostring(id)])
end

-- Section 3 hotkeys are recorded so that a synthetic Spaces shortcut colliding with
-- one of them (e.g. "Switch to Desktop 1" bound to Ctrl+Alt+1) is not swallowed by
-- Hammerspoon itself while it is being posted.
local workspaceHotkeys = {}

local function bindWorkspaceHotkey(mods, key, fn)
  local flags = 0
  for _, m in ipairs(mods) do
    flags = flags | (HOTKEY_MOD_FLAGS[m] or 0)
  end
  local hk = hs.hotkey.bind(mods, key, fn)
  table.insert(workspaceHotkeys, { hk = hk, keycode = hs.keycodes.map[key:lower()], flags = flags })
  return hk
end

-- -- Move operations ------------------------------------------------------------
local spaceMove = {
  op = nil,            -- active move operation (see moveWindowToTargetSpace)
  pending = nil,       -- queued request { kind = "delta" | "index", value = n }
  waitTimer = nil,     -- modifier-release poll timer (strong reference)
  waitDeadline = 0,
  requestedAt = nil,   -- when the first not-yet-executed request arrived
  lastModifierWait = nil,
  lastResult = nil,    -- outcome of the last move (see workspaceMover.lastResult())
}
local finishMove, startPendingMove, planSpaceSwitchKeys  -- forward declarations

-- Runs fn after sec seconds as part of op, holding a strong reference to the timer
-- (an unreferenced hs.timer.doAfter timer is garbage-collected and never fires,
-- which used to strand moves halfway). Lua errors abort the move cleanly.
local function opAfter(op, sec, fn)
  if op.done then return end
  local t
  t = hs.timer.doAfter(sec, function()
    op.timers[t] = nil
    if op.done then return end
    local ok, err = xpcall(fn, debug.traceback)
    if not ok then finishMove(op, false, "Lua error: " .. tostring(err)) end
  end)
  op.timers[t] = true
end

-- Polls cond() every interval seconds; calls onDone(true, value) as soon as it
-- returns a truthy value, or onDone(false) after timeout seconds.
local function opPoll(op, interval, timeout, cond, onDone)
  local deadline = hs.timer.secondsSinceEpoch() + timeout
  local function tick()
    local ok, value = pcall(cond)
    if ok and value then return onDone(true, value) end
    if hs.timer.secondsSinceEpoch() >= deadline then return onDone(false) end
    opAfter(op, interval, tick)
  end
  tick()
end

local function releaseMouse(op)
  if op.mouseDown then
    op.mouseDown = false
    postRawMouseButton(evTypes.leftMouseUp, op.dragPt or hs.mouse.absolutePosition(), op.evNum, 0, 0)
  end
end

-- Compact per-phase timeline, exposed as workspaceMover.lastResult().trace
local function trace(op, msg)
  table.insert(op.trace, string.format("+%.2fs %s", hs.timer.secondsSinceEpoch() - op.startedAt, msg))
end

local function restoreSuspendedHotkeys(op)
  for _, hk in ipairs(op.suspendedHotkeys or {}) do
    hk:enable()
  end
  op.suspendedHotkeys = nil
end

-- Posts leftMouseDragged events through pts, one every interval seconds.
local function dragMouseAlong(op, pts, interval, onDone)
  local i = 0
  local function step()
    i = i + 1
    local p = roundPt(pts[i])
    local prev = op.dragPt or p
    postRawMouseButton(evTypes.leftMouseDragged, p, op.evNum, p.x - prev.x, p.y - prev.y)
    op.dragPt = p
    if i >= #pts then return onDone() end
    opAfter(op, interval, step)
  end
  opAfter(op, interval, step)
end

local function dragMouseLine(op, fromPt, toPt, steps, interval, onDone)
  local pts = {}
  for i = 1, steps do
    table.insert(pts, {
      x = fromPt.x + (toPt.x - fromPt.x) * i / steps,
      y = fromPt.y + (toPt.y - fromPt.y) * i / steps,
    })
  end
  dragMouseAlong(op, pts, interval, onDone)
end

-- Posts a symbolic hotkey exactly as configured, then releases its modifiers so
-- Ctrl/Alt/Fn don't stay latched in the session modifier state.
local function postSymbolicHotkey(op, hotkey)
  local mods = hotkey.flags & MODIFIER_FLAGS_MASK
  for _, h in ipairs(workspaceHotkeys) do
    if h.keycode == hotkey.keycode and h.flags == mods and h.hk then
      h.hk:disable()
      op.suspendedHotkeys = op.suspendedHotkeys or {}
      table.insert(op.suspendedHotkeys, h.hk)
    end
  end
  local down = hs.eventtap.event.newKeyEvent(hotkey.keycode, true)
  down:rawFlags(hotkey.flags)
  down:setProperty(evProps.eventSourceUserData, SYNTHETIC_EVENT_TAG)
  down:post()
  local up = hs.eventtap.event.newKeyEvent(hotkey.keycode, false)
  up:rawFlags(hotkey.flags)
  up:setProperty(evProps.eventSourceUserData, SYNTHETIC_EVENT_TAG)
  up:post()
  local remaining = mods
  for _, mod in ipairs({ "shift", "cmd", "alt", "ctrl" }) do
    if (remaining & HOTKEY_MOD_FLAGS[mod]) ~= 0 then
      remaining = remaining & ~HOTKEY_MOD_FLAGS[mod]
      hs.eventtap.event.newKeyEvent(mod, false):rawFlags(remaining):post()
    end
  end
  if op.suspendedHotkeys then
    opAfter(op, 0.3, function() restoreSuspendedHotkeys(op) end)
  end
end

finishMove = function(op, ok, reason)
  if op.done then return end
  op.done = true
  for t in pairs(op.timers) do t:stop() end
  op.timers = {}
  pcall(releaseMouse, op)
  restoreSuspendedHotkeys(op)
  -- Never leave the user in Mission Control (no-op when it is already closed)
  if op.mcOpen then pcall(hs.spaces.closeMissionControl) end
  if op.savedCursor then hs.mouse.absolutePosition(op.savedCursor) end
  if op.dragTapWasEnabled and altDragTap then altDragTap:start() end
  if spaceMove.op == op then spaceMove.op = nil end

  if ok then
    -- An explicit move unpins the window (false, so the kitty auto-sticky watcher
    -- doesn't re-enroll it) and tells <Alt>Tab about its new workspace right away.
    stickyWindows[op.winId] = false
    if onWindowMovedToSpace then pcall(onWindowMovedToSpace, op.win, op.targetSpace) end
    pcall(function() op.win:focus() end)
    local okName, appName = pcall(function() return op.win:application():name() end)
    showBannerForSpace(op.scr, op.targetSpace, "moved " .. ((okName and appName) or "window"))
  else
    spaceMove.pending = nil
    hs.alert.show(string.format("Could not move window to Space %d", op.targetIdx or 0), 1.0)
  end
  spaceMove.lastResult = {
    ok = ok,
    reason = reason,
    method = op.method,
    winId = op.winId,
    fromSpace = op.fromSpace,
    targetSpace = op.targetSpace,
    targetIdx = op.targetIdx,
    winSpaces = hs.spaces.windowSpaces(op.winId),
    focusedSpace = hs.spaces.focusedSpace(),
    seconds = hs.timer.secondsSinceEpoch() - op.startedAt,
    modifierWait = op.modifierWait,
    trace = op.trace,
  }
  local msg = string.format("%s window %d -> Space %d (id %d) via %s in %.2fs: %s",
    ok and "moved" or "FAILED to move", op.winId, op.targetIdx or 0, op.targetSpace or 0,
    tostring(op.method), spaceMove.lastResult.seconds, tostring(reason))
  if ok then
    spaceMoveLog.i(msg)
  else
    spaceMoveLog.e(msg .. "\n  " .. table.concat(op.trace, "\n  "))
  end
  if spaceMove.pending then startPendingMove() end
end

-- -- B. Mission Control thumbnail drag (defined first: path A falls back to it) --
-- Mission Control is owned by the Dock (com.apple.WindowManager exposes nothing):
-- dock > "mc" > "mc.display" (AXDisplayID == hs.screen:id()) > "mc.windows" >
-- window thumbnail AXButtons (AXTitle = window title, no window id), and
-- "mc.spaces" > "mc.spaces.list" > one AXButton per Space ("exit to Desktop N")
-- in hs.spaces.allSpaces() order. Space buttons report collapsed, offscreen frames
-- (y < 0) until the Spaces bar expands.
local function mcChild(parent, identifier)
  for _, el in ipairs(parent or {}) do
    if el.AXIdentifier == identifier then return el end
  end
  return nil
end

local function getMissionControlDisplay(scr)
  local dock = hs.application.applicationsForBundleID("com.apple.dock")[1]
  local mc = mcChild(dock and hs.axuielement.applicationElement(dock), "mc")
  if not mc then return nil end
  local displays = {}
  for _, el in ipairs(mc) do
    if el.AXIdentifier == "mc.display" then table.insert(displays, el) end
  end
  local scrId, full = scr:id(), scr:fullFrame()
  for _, d in ipairs(displays) do
    if d.AXDisplayID == scrId then return d end
  end
  for _, d in ipairs(displays) do
    local df = d.AXFrame
    if df and df.x == full.x and df.y == full.y then return d end
  end
  return displays[1]
end

local function getMissionControlWindows(display)
  local list = {}
  for _, el in ipairs(mcChild(display, "mc.windows") or {}) do
    table.insert(list, el)
  end
  return list
end

local function getMissionControlSpaces(display)
  local list = {}
  for _, el in ipairs(mcChild(mcChild(display, "mc.spaces"), "mc.spaces.list") or {}) do
    table.insert(list, el)
  end
  return list
end

local function mcTitleMatches(candidate, title)
  if not candidate or not title then return false end
  if candidate == title then return true end
  local prefix, suffix = candidate:match("^(.-)…(.*)$")
  if not prefix or #prefix < 6 then return false end
  return title:sub(1, #prefix) == prefix and
         (suffix == "" or title:sub(#title - #suffix + 1) == suffix)
end

-- Mission Control titles drop the " - <App>[ - <profile>]" suffix that Chrome-style
-- browsers append to window titles ("Docs - Google Chrome - Marcel (Work)" shows
-- as "Docs"), so thumbnails are also matched against the title without it.
local function stripAppNameSuffix(title, appName)
  if not title or title == "" or not appName or appName == "" then return nil end
  local needle, cut, from = " - " .. appName, nil, 1
  while true do
    local s = title:find(needle, from, true)
    if not s then break end
    cut, from = s, s + 1
  end
  if cut and cut > 1 then return title:sub(1, cut - 1) end
  return nil
end

-- Picks win's thumbnail by title, then aspect ratio (thumbnails are scaled-down
-- copies), then Mission Control's front-to-back order (win was focused first).
-- Refuses to guess when neither title nor shape identifies the window.
local function findWindowThumbnail(thumbs, winTitle, winFrame, appName)
  local aspect = winFrame.w / math.max(1, winFrame.h)
  local shortTitle = stripAppNameSuffix(winTitle, appName)
  local scored = {}
  for idx, th in ipairs(thumbs) do
    local tf = th.AXFrame
    if tf and tf.w > 0 and tf.h > 0 then
      local title = th.AXTitle or ""
      local titleScore = 0
      if winTitle ~= "" and (title == winTitle or (shortTitle and title == shortTitle)) then
        titleScore = 2
      elseif winTitle ~= "" and (mcTitleMatches(title, winTitle) or
                                 (shortTitle and mcTitleMatches(title, shortTitle))) then
        titleScore = 1
      end
      local aspectErr = math.abs(math.log((tf.w / tf.h) / aspect))
      table.insert(scored, {
        el = th, frame = tf, idx = idx, titleScore = titleScore,
        aspectErr = aspectErr, shapeOk = aspectErr < 0.06,
      })
    end
  end
  table.sort(scored, function(a, b)
    if a.titleScore ~= b.titleScore then return a.titleScore > b.titleScore end
    if a.shapeOk ~= b.shapeOk then return a.shapeOk end
    return a.idx < b.idx
  end)
  local best, second = scored[1], scored[2]
  if not best then return nil end
  local unique = not second or second.titleScore < best.titleScore or
                 (best.shapeOk and not second.shapeOk)
  if best.titleScore > 0 or (best.shapeOk and unique) then
    return best.el, best.frame
  end
  return nil
end

local function isMissionControlOpen()
  local dock = hs.application.applicationsForBundleID("com.apple.dock")[1]
  return mcChild(dock and hs.axuielement.applicationElement(dock), "mc") ~= nil
end

-- Switches op.scr to op.targetSpace without Mission Control: the user's Spaces
-- shortcut(s) when enabled, else hs.spaces.gotoSpace(). Calls onDone(arrived).
local function switchToTargetSpaceDirectly(op, onDone)
  if hs.spaces.focusedSpace() == op.targetSpace then return onDone(true) end
  local function viaGotoSpace()
    pcall(hs.spaces.gotoSpace, op.targetSpace)
    opPoll(op, 0.05, 2.5, function() return hs.spaces.focusedSpace() == op.targetSpace end, onDone)
  end
  local plan = planSpaceSwitchKeys(op.scr, hs.spaces.focusedSpace(), op.targetSpace, op.targetIdx)
  if not plan then return viaGotoSpace() end
  local function runStep(i)
    local step = plan[i]
    if not step then return onDone(true) end
    postSymbolicHotkey(op, step.hotkey)
    opPoll(op, 0.03, 2.0, function() return hs.spaces.focusedSpace() == step.space end, function(switched)
      if switched then return runStep(i + 1) end
      viaGotoSpace()
    end)
  end
  runStep(1)
end

-- After the drop, leave Mission Control on the target Space. Mission Control
-- ignores a Space press while it is still animating (hs.spaces.gotoSpace() waits
-- hs.spaces.MCwaitTime after opening it for the same reason), and a press ~20ms
-- after the drop left it open on the 2560x1440 gMac. So let it settle first,
-- press again if it is still showing, and finally close it and switch Spaces
-- without it: the user must never be left in Mission Control.
local MC_SETTLE_SEC = 0.4
local MC_PRESS_ATTEMPTS = 2

local function followDroppedWindow(op, spaceListIdx)
  local attempt = 0
  local function pressTargetSpace()
    attempt = attempt + 1
    local sp = getMissionControlSpaces(getMissionControlDisplay(op.scr))[spaceListIdx]
    local okPress, pressed = pcall(function() return sp and sp:performAction("AXPress") end)
    trace(op, string.format("AXPress target Space (attempt %d): %s", attempt,
      (okPress and pressed) and "ok" or "failed"))
    opPoll(op, 0.05, 1.0, function()
      return hs.spaces.focusedSpace() == op.targetSpace
    end, function(arrived)
      if arrived then
        op.mcOpen = false
        return finishMove(op, true, "dragged in Mission Control")
      end
      if attempt < MC_PRESS_ATTEMPTS and isMissionControlOpen() then
        return opAfter(op, MC_SETTLE_SEC, pressTargetSpace)
      end
      trace(op, "Space press ignored; closing Mission Control and switching Spaces directly")
      pcall(hs.spaces.closeMissionControl)
      opPoll(op, 0.05, 1.5, function() return not isMissionControlOpen() end, function()
        op.mcOpen = isMissionControlOpen()
        switchToTargetSpaceDirectly(op, function(switched)
          finishMove(op, true, switched and "dragged in Mission Control, then switched Spaces directly"
            or "dragged in Mission Control (Space switch still pending)")
        end)
      end)
    end)
  end
  opAfter(op, MC_SETTLE_SEC, pressTargetSpace)
end

local function moveWindowViaMissionControl(op)
  local win, scr = op.win, op.scr
  op.method = (op.method and op.method ~= "mission-control") and (op.method .. "+mission-control") or "mission-control"
  local full = scr:fullFrame()
  local winTitle = win:title() or ""
  local winFrame = win:frame()
  local okApp, appName = pcall(function() return win:application():name() end)
  appName = okApp and appName or nil
  local spaceListIdx = indexOf(getRawSpacesForScreen(scr), op.targetSpace)
  if not spaceListIdx then
    return finishMove(op, false, "target Space is not on the window's display")
  end
  pcall(function() win:focus() end)
  hs.spaces.openMissionControl()
  op.mcOpen = true
  trace(op, string.format("Mission Control opened for %q", winTitle))
  local openedAt = hs.timer.secondsSinceEpoch()
  local lastKey, stableTicks = nil, 0

  -- 1. Wait for the opening animation to settle (presses mid-animation are ignored)
  opPoll(op, 0.05, 3.0, function()
    local display = getMissionControlDisplay(scr)
    local _, tf = findWindowThumbnail(getMissionControlWindows(display), winTitle, winFrame, appName)
    if not tf then return false end
    local key = string.format("%.0f,%.0f,%.0f,%.0f", tf.x, tf.y, tf.w, tf.h)
    stableTicks = (key == lastKey) and (stableTicks + 1) or 0
    lastKey = key
    return stableTicks >= 2 and (hs.timer.secondsSinceEpoch() - openedAt) >= 0.45 and tf
  end, function(ready, thumbFrame)
    trace(op, ready and string.format("thumbnail settled at %.0f,%.0f %.0fx%.0f",
      thumbFrame.x, thumbFrame.y, thumbFrame.w, thumbFrame.h) or "thumbnail not found")
    if not ready then
      return finishMove(op, false, "window thumbnail not found in Mission Control")
    end
    local startPt = roundPt(hs.geometry(thumbFrame).center)
    postRawMouseMove(startPt)
    opAfter(op, 0.05, function()
      op.dragPt = startPt
      postRawMouseButton(evTypes.leftMouseDown, startPt, op.evNum, 0, 0)
      op.mouseDown = true
      -- 2. Lift the thumbnail; the Spaces bar expands once a drag is in progress
      dragMouseAlong(op, {
        { x = startPt.x, y = startPt.y - 8 },
        { x = startPt.x, y = startPt.y - 30 },
        { x = startPt.x, y = startPt.y - 80 },
      }, 0.03, function()
        opPoll(op, 0.05, 1.5, function()
          local sp = getMissionControlSpaces(getMissionControlDisplay(scr))[spaceListIdx]
          local sf = sp and sp.AXFrame
          return sf and sf.y >= full.y and sf.h >= 30 and sf
        end, function(expanded, targetFrame)
          trace(op, expanded and string.format("Spaces bar expanded; target at %.0f,%.0f",
            targetFrame.x, targetFrame.y) or "Spaces bar did not expand")
          if not expanded then
            return finishMove(op, false, "Mission Control Spaces bar did not expand")
          end
          -- 3. Drop onto the expanded target Space thumbnail
          local endPt = roundPt(hs.geometry(targetFrame).center)
          dragMouseLine(op, op.dragPt, endPt, 6, 0.025, function()
            opAfter(op, 0.2, function()
              dragMouseAlong(op, { { x = endPt.x + 1, y = endPt.y } }, 0.01, function()
                opAfter(op, 0.06, function()
                  releaseMouse(op)
                  -- 4. Verify, then follow the window to the target Space
                  opPoll(op, 0.05, 1.5, function()
                    return indexOf(hs.spaces.windowSpaces(win) or {}, op.targetSpace)
                  end, function(dropped)
                    trace(op, dropped and "drop moved the window" or "drop did not move the window")
                    if not dropped then
                      return finishMove(op, false, "Mission Control drop did not move the window")
                    end
                    followDroppedWindow(op, spaceListIdx)
                  end)
                end)
              end)
            end)
          end)
        end)
      end)
    end)
  end)
end

-- -- A. Titlebar drag + the user's macOS Spaces shortcut ---------------------------
local INTERACTIVE_AX_ROLES = {
  AXButton = true, AXPopUpButton = true, AXMenuButton = true, AXRadioButton = true,
  AXCheckBox = true, AXTextField = true, AXTextArea = true, AXComboBox = true,
  AXSearchField = true, AXSlider = true, AXIncrementor = true, AXLink = true,
  AXTab = true, AXDisclosureTriangle = true, AXSegmentedControl = true,
  AXMenuBar = true, AXMenuBarItem = true,
}

-- Returns a titlebar point that drags the window frame itself (not a traffic light,
-- toolbar button or browser tab), validated with an Accessibility hit-test. In
-- Chrome the strip right of the zoom button is the draggable caption, while ~30px
-- further right is the "Tab search" button and then the tabs.
local function findTitlebarGrabPoint(win)
  local zoom = win:zoomButtonRect()
  if not (zoom and zoom.w and zoom.w > 0) then return nil end
  local f = win:frame()
  local full = (win:screen() or hs.screen.mainScreen()):fullFrame()
  local y = math.floor(zoom.y + zoom.h / 2)
  local xs = { zoom.x + zoom.w + 10, zoom.x + zoom.w + 4, f.x + f.w / 2 }
  for x = zoom.x + zoom.w + 60, f.x + f.w - 60, 80 do
    table.insert(xs, x)
  end
  local app = win:application()
  local appPid = app and app:pid()
  -- Never hit-test our own process over AX (it would deadlock until the AX timeout)
  local canHitTest = not (app and app:bundleID() == hs.processInfo.bundleID)
  local sys = hs.axuielement.systemWideElement()
  local firstCandidate, anyHitTest = nil, false
  for _, x in ipairs(xs) do
    local pt = { x = math.floor(x), y = y }
    local inside = pt.x > f.x + 2 and pt.x < f.x + f.w - 2 and pt.y > f.y and
      pt.x >= full.x and pt.x < full.x + full.w and pt.y >= full.y and pt.y < full.y + full.h
    if inside then
      firstCandidate = firstCandidate or pt
      if not canHitTest then return pt end
      local ok, role, pid = pcall(function()
        local el = sys:elementAtPosition(pt.x, pt.y)
        if not el then return nil end
        return el.AXRole or "", el:pid()
      end)
      if ok and role then
        anyHitTest = true
        if pid == appPid and not INTERACTIVE_AX_ROLES[role] then
          return pt
        end
      end
    end
  end
  -- Apps without AX hit-testing: rely on the drag-engagement check instead
  if not anyHitTest then return firstCandidate end
  return nil
end

-- Keystrokes that switch scr from fromSpace to targetSpace, each paired with the
-- Space it lands on: one "Move left/right a space" hop for adjacent Spaces, else
-- a direct "Switch to Desktop N" shortcut when enabled (single display only, where
-- Desktop N is our workspace N), else one hop per Space.
-- (Forward-declared above: the Mission Control path also uses it.)
planSpaceSwitchKeys = function(scr, fromSpace, targetSpace, targetIdx)
  local onScreen = getRawSpacesForScreen(scr)
  local fromPos, toPos = indexOf(onScreen, fromSpace), indexOf(onScreen, targetSpace)
  if not fromPos or not toPos or fromPos == toPos then return nil end
  local hop = getSymbolicHotkey((toPos > fromPos) and SYMBOLIC_MOVE_RIGHT_SPACE or SYMBOLIC_MOVE_LEFT_SPACE)
  if hop and math.abs(toPos - fromPos) == 1 then
    return { { hotkey = hop, space = targetSpace } }
  end
  local sharedSpaces = (type(hs.spaces.screensHaveSeparateSpaces) == "function") and
                       (not hs.spaces.screensHaveSeparateSpaces())
  if targetIdx and targetIdx <= 16 and (#hs.screen.allScreens() == 1 or sharedSpaces) then
    local direct = getSymbolicHotkey(SYMBOLIC_SWITCH_TO_DESKTOP_1 + targetIdx - 1)
    if direct then return { { hotkey = direct, space = targetSpace } } end
  end
  if not hop then return nil end
  local plan = {}
  local step = (toPos > fromPos) and 1 or -1
  for pos = fromPos + step, toPos, step do
    table.insert(plan, { hotkey = hop, space = onScreen[pos] })
  end
  return plan
end

local function moveViaTitlebarDrag(op, grabPt, plan)
  local win = op.win
  op.method = "titlebar-drag"
  trace(op, string.format("titlebar drag at %d,%d with %d Spaces shortcut step(s)", grabPt.x, grabPt.y, #plan))

  local function settle()
    releaseMouse(op)
    opAfter(op, 0.06, function()
      pcall(function() win:setFrame(op.origFrame) end)  -- undo the nudge (and Chrome's drag-unmaximize)
      local winSpaces = hs.spaces.windowSpaces(win) or {}
      local here = hs.spaces.activeSpaceOnScreen(op.scr) or hs.spaces.focusedSpace()
      trace(op, "released; window on " .. table.concat(winSpaces, ",") .. ", active Space " .. tostring(here))
      if indexOf(winSpaces, op.targetSpace) then
        return finishMove(op, true, "carried along by the macOS Spaces shortcut")
      elseif indexOf(winSpaces, here) then
        -- Shortcut disabled/ignored or a hop fell short: the window is still with us
        op.fromSpace = here
        spaceMoveLog.w("titlebar drag + Spaces shortcut fell short; continuing via Mission Control")
        return opAfter(op, 0.15, function() moveWindowViaMissionControl(op) end)
      end
      if winSpaces[1] then pcall(hs.spaces.gotoSpace, winSpaces[1]) end
      finishMove(op, false, "window did not follow the Space switch (drag interrupted?)")
    end)
  end

  local function runStep(i)
    local step = plan[i]
    if not step then
      return opPoll(op, 0.03, 0.8, function()
        return indexOf(hs.spaces.windowSpaces(win) or {}, op.targetSpace)
      end, settle)
    end
    trace(op, string.format("post shortcut keycode %d flags 0x%X (expect Space %d)",
      step.hotkey.keycode, step.hotkey.flags, step.space))
    postSymbolicHotkey(op, step.hotkey)
    opPoll(op, 0.03, 2.5, function()
      return hs.spaces.focusedSpace() == step.space
    end, function(switched)
      trace(op, switched and ("now on Space " .. step.space) or "shortcut had no effect")
      if switched then return runStep(i + 1) end
      symbolicHotkeysCache = nil  -- the user may have changed the shortcut; re-read next time
      settle()
    end)
  end

  postRawMouseMove(grabPt)
  opAfter(op, 0.03, function()
    op.dragPt = { x = grabPt.x, y = grabPt.y }
    postRawMouseButton(evTypes.leftMouseDown, op.dragPt, op.evNum, 0, 0)
    op.mouseDown = true
    dragMouseAlong(op, { { x = grabPt.x + 2, y = grabPt.y }, { x = grabPt.x + 4, y = grabPt.y } }, 0.035, function()
      -- Only switch Spaces once WindowServer is really dragging the window
      opPoll(op, 0.02, 0.35, function()
        local fr, o = win:frame(), op.origFrame
        return fr.x ~= o.x or fr.y ~= o.y or fr.w ~= o.w or fr.h ~= o.h
      end, function(engaged)
        trace(op, engaged and "window drag engaged" or "window drag did not engage")
        if engaged then return runStep(1) end
        releaseMouse(op)
        spaceMoveLog.w("titlebar drag did not engage; falling back to Mission Control")
        opAfter(op, 0.15, function() moveWindowViaMissionControl(op) end)
      end)
    end)
  end)
end

-- -- Entry points -----------------------------------------------------------------
local function moveWindowToTargetSpace(win, scr, fromSpace, targetSpace, targetIdx)
  -- Native API first: works on macOS < 14.5 (a silent NOP returning true on 14.5+)
  pcall(hs.spaces.moveWindowToSpace, win:id(), targetSpace)
  local nowSpaces = hs.spaces.windowSpaces(win)
  if nowSpaces and #nowSpaces == 1 and nowSpaces[1] == targetSpace then
    hs.spaces.gotoSpace(targetSpace)
    stickyWindows[win:id()] = false
    if onWindowMovedToSpace then pcall(onWindowMovedToSpace, win, targetSpace) end
    win:focus()
    spaceMove.lastResult = { ok = true, method = "native", winId = win:id(), targetSpace = targetSpace, targetIdx = targetIdx }
    local app = win:application()
    showBannerForSpace(scr, targetSpace, "moved " .. ((app and app:name()) or "window"))
    return
  end

  local op = {
    win = win, winId = win:id(), scr = scr,
    fromSpace = fromSpace, targetSpace = targetSpace, targetIdx = targetIdx,
    origFrame = win:frame(), timers = {}, evNum = allocMouseEventNumber(),
    startedAt = hs.timer.secondsSinceEpoch(), savedCursor = hs.mouse.absolutePosition(),
    trace = {}, modifierWait = spaceMove.lastModifierWait,
  }
  op.dragTapWasEnabled = (altDragTap and altDragTap:isEnabled()) or false
  if op.dragTapWasEnabled then altDragTap:stop() end
  spaceMove.op = op
  opAfter(op, 15, function() finishMove(op, false, "watchdog timeout") end)
  opAfter(op, 0, function()
    local plan = planSpaceSwitchKeys(scr, fromSpace, targetSpace, targetIdx)
    local grabPt = plan and findTitlebarGrabPoint(win)
    local app = win:application()
    trace(op, string.format("%s %q: Space %d -> %d, shortcut plan: %s, titlebar grab: %s",
      app and app:name() or "?", win:title() or "", fromSpace, targetSpace,
      plan and (#plan .. " step(s)") or "none", grabPt and (grabPt.x .. "," .. grabPt.y) or "none"))
    if grabPt then
      moveViaTitlebarDrag(op, grabPt, plan)
    else
      moveWindowViaMissionControl(op)
    end
  end)
end

local function executeMoveRequest(req)
  local win = hs.window.focusedWindow()
  if not win then return end
  if win:isFullScreen() then
    hs.alert.show("Fullscreen windows can't be moved between Spaces", 0.8)
    return
  end
  local scr = win:screen() or hs.screen.mainScreen()
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  local currentSpace = hs.spaces.activeSpaceOnScreen(scr) or hs.spaces.focusedSpace()
  local currentIdx = indexOf(spaces, currentSpace)
  local targetIdx
  if req.kind == "delta" then
    if not currentIdx then return end
    targetIdx = currentIdx + req.value
    if targetIdx > #spaces and #spaces < 6 then
      spaces = ensureUserSpacesCount(scr, math.min(targetIdx, 6))
    end
    targetIdx = math.max(1, math.min(targetIdx, #spaces))
    if targetIdx == currentIdx then
      showWorkspaceBanner(scr, currentIdx, #spaces,
        string.format("Already at Space %d of %d", currentIdx, #spaces))
      return
    end
  else
    targetIdx = req.value
    if #spaces < targetIdx and targetIdx <= 6 then
      spaces = ensureUserSpacesCount(scr, targetIdx)
    end
    if not spaces[targetIdx] then
      hs.alert.show(string.format("Space %d does not exist", targetIdx), 0.7)
      return
    end
    if spaces[targetIdx] == currentSpace then
      showWorkspaceBanner(scr, targetIdx, #spaces)
      return
    end
  end
  moveWindowToTargetSpace(win, scr, currentSpace, spaces[targetIdx], targetIdx)
end

-- The hotkey fires while Ctrl/Alt/Shift are still physically held (~100-250ms).
-- Posting synthetic events then would mix those modifiers in (Ctrl+click = right
-- click, Ctrl+Alt+Shift+Arrow = our own hotkey), and posting a synthetic "alt up"
-- does not release a physically held key, so wait for the real release first.
local MODIFIER_RELEASE_TIMEOUT = 2.0

local function hotkeyModifiersHeld()
  local mods = hs.eventtap.checkKeyboardModifiers()
  return (mods.ctrl or mods.alt or mods.shift or mods.cmd) and true or false
end

startPendingMove = function()
  if spaceMove.op or spaceMove.waitTimer or not spaceMove.pending then return end
  local function poll()
    spaceMove.waitTimer = nil
    if hotkeyModifiersHeld() and hs.timer.secondsSinceEpoch() < spaceMove.waitDeadline then
      spaceMove.waitTimer = hs.timer.doAfter(0.015, poll)
      return
    end
    local req = spaceMove.pending
    spaceMove.pending = nil
    if not req then return end
    spaceMove.lastModifierWait = hs.timer.secondsSinceEpoch() - (spaceMove.requestedAt or 0)
    spaceMove.requestedAt = nil
    local ok, err = xpcall(executeMoveRequest, debug.traceback, req)
    if not ok then
      spaceMoveLog.e("move request failed: " .. tostring(err))
      if spaceMove.op then finishMove(spaceMove.op, false, "Lua error: " .. tostring(err)) end
    end
  end
  poll()
end

local function requestSpaceMove(kind, value)
  local pending = spaceMove.pending
  if pending and pending.kind == "delta" and kind == "delta" then
    pending.value = pending.value + value
  else
    spaceMove.pending = { kind = kind, value = value }
  end
  spaceMove.requestedAt = spaceMove.requestedAt or hs.timer.secondsSinceEpoch()
  spaceMove.waitDeadline = hs.timer.secondsSinceEpoch() + MODIFIER_RELEASE_TIMEOUT
  startPendingMove()
end

local workspaceSwitchTimer = nil

local function switchToWorkspaceIndex(idx)
  local scr = hs.screen.mainScreen()
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  if #spaces < idx and idx <= 6 then
    spaces = ensureUserSpacesCount(scr, idx)
  end
  local targetSpace = spaces[idx]
  if targetSpace then
    if targetSpace == hs.spaces.activeSpaceOnScreen(scr) then
      showWorkspaceBanner(scr, idx, #spaces)
      return
    end
    armSwitchBanner(scr, 3.0)
    -- Deferred so the hotkey callback returns before gotoSpace() busy-waits on Mission Control
    if workspaceSwitchTimer then workspaceSwitchTimer:stop() end
    workspaceSwitchTimer = hs.timer.doAfter(0.01, function()
      workspaceSwitchTimer = nil
      hs.spaces.gotoSpace(targetSpace)
    end)
  end
end

-- Hammerspoon's own <Ctrl><Alt>Left/Right: switch to the neighbouring Desktop.
local function switchToAdjacentWorkspace(delta)
  local scr = hs.screen.mainScreen()
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  local cur = indexOf(spaces, hs.spaces.activeSpaceOnScreen(scr) or hs.spaces.focusedSpace())
  if not cur then return end
  local target = cur + delta
  if target < 1 or target > #spaces then
    showWorkspaceBanner(scr, cur, #spaces, string.format("Already at Space %d of %d", cur, #spaces))
    return
  end
  switchToWorkspaceIndex(target)
end

local function moveFocusedWindowToWorkspaceIndex(idx)
  requestSpaceMove("index", idx)
end

local function moveFocusedWindowToAdjacentSpace(delta)
  requestSpaceMove("delta", delta)
end

-- -- Desktop-switch shortcuts shared with macOS -----------------------------------
-- <Ctrl><Alt>1..6 and <Ctrl><Alt>Left/Right switch Desktops. When macOS's own
-- "Switch to Desktop N" (118..123) or "Move left/right a space" (79/81) shortcut
-- is enabled with the *same* keys for the same action, macOS does the switch
-- natively (~0.3s, no Mission Control flash) and Hammerspoon only shows the
-- banner once the Space has changed:
--   * its own hotkey for that binding stays disabled (macOS fires "Switch to
--     Desktop N" alongside a Hammerspoon hotkey, which used to add a slow
--     Mission Control gotoSpace(); and it holds Ctrl+Alt+Arrow exclusively, so a
--     Hammerspoon hotkey there cannot even be registered), and
--   * a passive key tap that never consumes events notices the keypress.
-- Bindings macOS does not share keep the Hammerspoon hotkey (gotoSpace + banner).
-- Ownership is re-checked when com.apple.symbolichotkeys.plist changes on disk.
local switchBindings = {}

-- Pure: does the com.apple.symbolichotkeys entry for symbolicId (nil = absent)
-- bind exactly this keycode + modifiers (fn and other non-modifier bits ignored)?
local function symbolicEntryMatches(symbolicId, entry, keycode, modFlags)
  local hk = symbolicHotkeyFromEntry(symbolicId, entry)
  return hk ~= nil and hk.keycode == keycode and (hk.flags & MODIFIER_FLAGS_MASK) == modFlags
end

local function macOSOwnsBinding(b)
  return symbolicEntryMatches(b.symbolicId, readSymbolicHotkeys()[tostring(b.symbolicId)], b.keycode, b.flags)
end

local function addSwitchBinding(mods, key, symbolicId, action, fn)
  local flags = 0
  for _, m in ipairs(mods) do
    flags = flags | (HOTKEY_MOD_FLAGS[m] or 0)
  end
  local hk = hs.hotkey.new(mods, key, fn)
  table.insert(_G.HS_KEEPALIVE.hotkeys, hk)
  table.insert(switchBindings, {
    key = key, keycode = hs.keycodes.map[key:lower()], flags = flags,
    symbolicId = symbolicId, action = action, hk = hk, enabled = false, ownedByMacOS = nil,
  })
end

-- Enables each Hammerspoon hotkey only where macOS does not bind the same keys.
local function syncSwitchBindings()
  for _, b in ipairs(switchBindings) do
    local owned = macOSOwnsBinding(b)
    if owned and b.enabled then
      b.hk:disable()
      b.enabled = false
    elseif not owned and not b.enabled then
      b.enabled = b.hk:enable() ~= nil
    end
    if owned ~= b.ownedByMacOS then
      spaceMoveLog.i(string.format("Ctrl+Alt+%s: %s", b.key, owned
        and "macOS shortcut has the same keys; macOS switches, Hammerspoon shows the banner"
        or "handled by Hammerspoon"))
    end
    b.ownedByMacOS = owned
  end
end

-- macOS performs the switch itself: show the banner once it lands, or right away
-- when nothing will move (already on Desktop N). At the first/last Desktop, macOS
-- 26 wraps around ("Move left" on Desktop 1 goes to the last one) while older
-- versions stay put, so wait to see which happened before saying "Already at".
local switchEdgeTimer = nil

local function onMacOSSwitchShortcut(action)
  local scr = hs.screen.mainScreen()
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  local current = hs.spaces.activeSpaceOnScreen(scr) or hs.spaces.focusedSpace()
  local cur = indexOf(spaces, current)
  if action.kind == "index" then
    if spaces[action.idx] == current then
      return showWorkspaceBanner(scr, action.idx, #spaces)
    end
    if not spaces[action.idx] then
      -- macOS ignores Desktops that do not exist; create it and switch ourselves
      return switchToWorkspaceIndex(action.idx)
    end
  elseif cur and (cur + action.delta < 1 or cur + action.delta > #spaces) then
    if switchEdgeTimer then switchEdgeTimer:stop() end
    switchEdgeTimer = hs.timer.doAfter(0.9, function()
      switchEdgeTimer = nil
      if (hs.spaces.activeSpaceOnScreen(scr) or hs.spaces.focusedSpace()) == current then
        showWorkspaceBanner(scr, cur, #spaces, string.format("Already at Space %d of %d", cur, #spaces))
      end
    end)
  end
  armSwitchBanner(scr, 1.6)
end

for i = 1, 6 do
  addSwitchBinding({"ctrl", "alt"}, tostring(i), SYMBOLIC_SWITCH_TO_DESKTOP_1 + i - 1,
    { kind = "index", idx = i }, function() switchToWorkspaceIndex(i) end)
end
addSwitchBinding({"ctrl", "alt"}, "Left", SYMBOLIC_MOVE_LEFT_SPACE,
  { kind = "delta", delta = -1 }, function() switchToAdjacentWorkspace(-1) end)
addSwitchBinding({"ctrl", "alt"}, "Right", SYMBOLIC_MOVE_RIGHT_SPACE,
  { kind = "delta", delta = 1 }, function() switchToAdjacentWorkspace(1) end)
syncSwitchBindings()

local switchShortcutTimer = nil
local switchShortcutTap = hs.eventtap.new({ evTypes.keyDown }, function(e)
  local flags = e:getFlags()
  if not (flags.ctrl and flags.alt) or flags.cmd or flags.shift then return false end
  if e:getProperty(evProps.eventSourceUserData) == SYNTHETIC_EVENT_TAG or spaceMove.op then
    return false
  end
  local keycode = e:getKeyCode()
  for _, b in ipairs(switchBindings) do
    if b.keycode == keycode then
      if macOSOwnsBinding(b) ~= b.ownedByMacOS then syncSwitchBindings() end
      local action = b.action
      if b.ownedByMacOS then
        -- Let the keystroke through to macOS; banner logic runs after the tap returns
        switchShortcutTimer = hs.timer.doAfter(0, function()
          switchShortcutTimer = nil
          onMacOSSwitchShortcut(action)
        end)
      elseif not b.enabled then
        -- Neither macOS nor a Hammerspoon hotkey handles it (registration failed)
        switchShortcutTimer = hs.timer.doAfter(0, function()
          switchShortcutTimer = nil
          if action.kind == "index" then switchToWorkspaceIndex(action.idx)
          else switchToAdjacentWorkspace(action.delta) end
        end)
        return true
      end
      return false
    end
  end
  return false
end)
switchShortcutTap:start()
_G.HS_KEEPALIVE.taps.switchShortcutTap = switchShortcutTap

-- Re-check which shortcuts macOS owns whenever System Settings rewrites the plist
local switchShortcutsWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/Library/Preferences", function(paths)
  for _, p in ipairs(paths) do
    if p:find("com.apple.symbolichotkeys.plist", 1, true) then
      symbolicHotkeysCache = nil
      syncSwitchBindings()
      return
    end
  end
end)
switchShortcutsWatcher:start()
_G.HS_KEEPALIVE.watchers.switchShortcutsWatcher = switchShortcutsWatcher

for i = 1, 6 do
  bindWorkspaceHotkey({"ctrl", "shift", "alt"}, tostring(i), function()
    moveFocusedWindowToWorkspaceIndex(i)
  end)
end

bindWorkspaceHotkey({"ctrl", "shift", "alt"}, "Left",  function() moveFocusedWindowToAdjacentSpace(-1) end)
bindWorkspaceHotkey({"ctrl", "shift", "alt"}, "Right", function() moveFocusedWindowToAdjacentSpace(1)  end)

-- Pre-warm the Spaces-shortcut cache off the config-load path (`defaults export` ~50ms).
-- Declared before assignment so the callback captures (and thus keeps alive) the timer.
local symbolicHotkeysWarmTimer
symbolicHotkeysWarmTimer = hs.timer.doAfter(1.0, function()
  symbolicHotkeysWarmTimer = nil
  pcall(readSymbolicHotkeys)
end)

-- Live debugging / testing over IPC (hs.ipc is loaded at the top), e.g.:
--   hs -c 'workspaceMover.moveFocusedWindowToAdjacentSpace(1)'
--   hs -c 'return hs.inspect(workspaceMover.lastResult())'
workspaceMover = {
  switchToWorkspaceIndex = switchToWorkspaceIndex,
  moveFocusedWindowToWorkspaceIndex = moveFocusedWindowToWorkspaceIndex,
  moveFocusedWindowToAdjacentSpace = moveFocusedWindowToAdjacentSpace,
  ensureUserSpacesCount = ensureUserSpacesCount,
  getSymbolicHotkey = getSymbolicHotkey,
  -- Pure helpers, exercised by ~/scripts/tests/test_macos_migration.sh over IPC
  symbolicHotkeyFromEntry = symbolicHotkeyFromEntry,
  symbolicEntryMatches = symbolicEntryMatches,
  findWindowThumbnail = findWindowThumbnail,
  -- e.g. hs -c 'return hs.inspect(workspaceMover.switchShortcutStatus())'
  switchShortcutStatus = function()
    local out = {}
    for _, b in ipairs(switchBindings) do
      out["Ctrl+Alt+" .. b.key] = b.ownedByMacOS and "macOS (banner only)"
        or (b.enabled and "Hammerspoon hotkey" or "Hammerspoon via key tap")
    end
    out.keyTap = switchShortcutTap:isEnabled() and "enabled" or "DISABLED"
    return out
  end,
  showWorkspaceBanner = showWorkspaceBanner,
  isBannerVisible = function() return workspaceBanner ~= nil and workspaceBanner:isShowing() end,
  isBusy = function()
    return spaceMove.op ~= nil or spaceMove.waitTimer ~= nil or spaceMove.pending ~= nil
  end,
  lastResult = function() return spaceMove.lastResult end,
}

-- -----------------------------------------------------------------------------
-- 4. Sticky Window Toggle (<Alt>F6) & Auto-Sticky Kitty Watcher
-- -----------------------------------------------------------------------------
-- stickyWindows is declared in Section 3 so that explicit <Ctrl><Shift><Alt> moves
-- can unpin a window (false = explicitly unpinned, never auto-re-enrolled below).
local autoStickyApps = {["kitty"] = true}

local function syncStickyWindowsToFocusedSpace()
  local activeSpace = hs.spaces.focusedSpace()
  if not activeSpace then return end

  -- Auto-enroll any Kitty window if configured
  for appName, enabled in pairs(autoStickyApps) do
    if enabled then
      local app = hs.application.get(appName)
      if app then
        for _, w in ipairs(app:allWindows()) do
          if w:isStandard() and stickyWindows[w:id()] == nil then
            stickyWindows[w:id()] = true
          end
        end
      end
    end
  end

  for winId, isPinned in pairs(stickyWindows) do
    if isPinned then
      local w = hs.window.get(winId)
      if w then
        hs.spaces.moveWindowToSpace(winId, activeSpace)
      else
        stickyWindows[winId] = nil
      end
    end
  end
end

  local stickySyncTimer = nil
  local spaceWatcher = hs.spaces.watcher.new(function(_)
    if stickySyncTimer then stickySyncTimer:stop() end
    stickySyncTimer = hs.timer.doAfter(0.05, function()
      stickySyncTimer = nil
      syncStickyWindowsToFocusedSpace()
    end)
    _G.HS_KEEPALIVE.timers.stickySyncTimer = stickySyncTimer
  end)
  spaceWatcher:start()
  _G.HS_KEEPALIVE.watchers.spaceWatcher = spaceWatcher

  keepHotkey({"alt"}, "f6", function()
  local win = hs.window.focusedWindow()
  if not win then return end
  local winId = win:id()
  if stickyWindows[winId] then
    stickyWindows[winId] = false
    hs.alert.show("Unpinned window from all Spaces: " .. (win:application():name() or ""))
  else
    stickyWindows[winId] = true
    hs.alert.show("Pinned window across all Spaces: " .. (win:application():name() or ""))
  end
end)

-- -----------------------------------------------------------------------------
-- 5. Borderless Alt+Left-Drag Move & Alt+Right-Drag Resize (easy_click=Alt)
-- -----------------------------------------------------------------------------
local dragState = {
  mode = nil,
  win = nil,
  startMouse = nil,
  startFrame = nil,
}

local function getWindowUnderMouse(pt)
  local orderedWins = hs.window.orderedWindows()
  for _, w in ipairs(orderedWins) do
    if w:isVisible() and w:isStandard() then
      local f = w:frame()
      if pt.x >= f.x and pt.x <= (f.x + f.w) and pt.y >= f.y and pt.y <= (f.y + f.h) then
        return w
      end
    end
  end
  return hs.window.focusedWindow()
end

altDragTap = hs.eventtap.new({
  hs.eventtap.event.types.leftMouseDown,
  hs.eventtap.event.types.leftMouseDragged,
  hs.eventtap.event.types.leftMouseUp,
  hs.eventtap.event.types.rightMouseDown,
  hs.eventtap.event.types.rightMouseDragged,
  hs.eventtap.event.types.rightMouseUp,
}, function(e)
  local flags = e:getFlags()
  local evType = e:getType()

  if evType == hs.eventtap.event.types.leftMouseUp or
     evType == hs.eventtap.event.types.rightMouseUp then
    if dragState.mode ~= nil then
      dragState.mode = nil
      dragState.win = nil
      return true
    end
    return false
  end

  if not (flags.alt and not flags.cmd and not flags.ctrl and not flags.shift) then
    dragState.mode = nil
    dragState.win = nil
    return false
  end

  local mousePt = hs.mouse.absolutePosition()

  if evType == hs.eventtap.event.types.leftMouseDown then
    local targetWin = getWindowUnderMouse(mousePt)
    if targetWin then
      dragState.mode = "move"
      dragState.win = targetWin
      dragState.startMouse = mousePt
      dragState.startFrame = targetWin:frame()
      targetWin:focus()
      return true
    end
  elseif evType == hs.eventtap.event.types.rightMouseDown then
    local targetWin = getWindowUnderMouse(mousePt)
    if targetWin then
      dragState.mode = "resize"
      dragState.win = targetWin
      dragState.startMouse = mousePt
      dragState.startFrame = targetWin:frame()
      targetWin:focus()
      return true
    end
  elseif evType == hs.eventtap.event.types.leftMouseDragged and dragState.mode == "move" and dragState.win then
    local dx = mousePt.x - dragState.startMouse.x
    local dy = mousePt.y - dragState.startMouse.y
    dragState.win:setFrame({
      x = dragState.startFrame.x + dx,
      y = dragState.startFrame.y + dy,
      w = dragState.startFrame.w,
      h = dragState.startFrame.h,
    }, 0)
    return true
  elseif evType == hs.eventtap.event.types.rightMouseDragged and dragState.mode == "resize" and dragState.win then
    local dx = mousePt.x - dragState.startMouse.x
    local dy = mousePt.y - dragState.startMouse.y
    dragState.win:setFrame({
      x = dragState.startFrame.x,
      y = dragState.startFrame.y,
      w = math.max(200, dragState.startFrame.w + dx),
      h = math.max(120, dragState.startFrame.h + dy),
    }, 0)
    return true
  end

  return false
end)
altDragTap:start()

-- -----------------------------------------------------------------------------
-- 6. <Alt>Tab Fuzzy Window Switcher with Multi-Workspace Prefix Filter & Border
-- -----------------------------------------------------------------------------
-- Replicates ~/modules/x-window-shortcuts/rofi-window-switcher,
-- fzf-window-switcher.sh, and draw_border_around_window.py with O(1) startup:
--   - Background async indexing tracks windows across all workspaces (1..6) in
--     knownWindowsById and caches app icons in appIconCache (like USER_ICONS).
--   - Pressing <Alt>Tab only queries the active Space's windows (the single
--     Accessibility pass hs.window.orderedWindows() makes, ~10ms) and reads
--     other workspaces from in-memory cache (zero hs.window.get or
--     WindowServer scans of other Spaces on startup).
--   - Minimized windows are listed too, tagged [Minimized], under the
--     workspace they were minimized on and after all other windows (so they
--     are never the default <Alt>Tab -> Enter target); choosing one restores it.
--   - Choosing a window focuses exactly that window -- never its app's
--     last-used window, which may be on another workspace -- and switches
--     workspaces first only when the chosen window lives on another one.
--   - Reuses a single pre-allocated hs.canvas border window (highlightCanvas)
--     and debounces border updates asynchronously (40ms throttle via
--     borderDebounceTimer) using pre-cached window frames.
--   - Opens with default prefix filter "^<current_workspace_num> " (with
--     trailing space). Pressing Backspace once (turning "^1 " into "^1" or "")
--     immediately shows all windows from all workspaces (1..6) without
--     mutating NSTextField inside queryChangedCallback.
local mruWindowIds = hs.settings.get("hammerspoonMruIds") or {}
local knownWindowsById = {}
local persistedWinMeta = hs.settings.get("hammerspoonWindowCache") or {}
for k, v in pairs(persistedWinMeta) do
  local numId = tonumber(k)
  if numId and type(v) == "table" then
    knownWindowsById[numId] = v
  end
end

local appIconCache = {}
local cachedSpaceIndexById = {}
local cachedCurrentSpaceId = nil
local cachedCurrentSpaceIdx = 1
local persistStateTimer = nil
local bgSpaceSyncTimer = nil

local highlightCanvas = nil
local borderDebounceTimer = nil
local BORDER_DEBOUNCE_SEC = 0.04

local windowChooser = nil
local chooserPollTimer = nil
local lastSelectedRow = nil
local allChooserChoices = {}
local initialChooserQuery = ""
local preChooserFocusedWindow = nil  -- focused when <Alt>Tab opened the chooser (refocused on Escape)
local switcherFocusTimer = nil       -- strong reference to the pending focus hand-off step
local switcherFocusToken = 0         -- bumped to cancel a pending focus hand-off

local PREFIXED_APP_SHORTNAMES = {"kitty", "wezterm", "chrome", "emacs"}

local IGNORED_APP_NAMES = {
  ["Dock"] = true,
  ["WindowServer"] = true,
  ["Control Center"] = true,
  ["Notification Center"] = true,
  ["SystemUIServer"] = true,
  ["Spotlight"] = true,
  ["SwiftBar"] = true,
  ["Karabiner-NotificationWindow"] = true,
  ["Hammerspoon"] = true,
}

local function normalizeShortAppPrefix(appName)
  local lower = string.lower(appName or "")
  for _, prefix in ipairs(PREFIXED_APP_SHORTNAMES) do
    if lower:find(prefix, 1, true) then
      return prefix
    end
  end
  return nil
end

local function getCachedAppIcon(bundleID)
  if not bundleID or bundleID == "" then return nil end
  local cached = appIconCache[bundleID]
  if cached ~= nil then
    return cached or nil
  end
  local icon = hs.image.imageFromAppBundle(bundleID)
  appIconCache[bundleID] = icon or false
  return icon
end

local function schedulePersistWindowTrackerState()
  if persistStateTimer then
    persistStateTimer:stop()
  end
  persistStateTimer = hs.timer.doAfter(2.0, function()
    persistStateTimer = nil
    local serializableMeta = {}
    for winId, meta in pairs(knownWindowsById) do
      serializableMeta[tostring(winId)] = {
        winId = winId,
        title = meta.title or "",
        appName = meta.appName or "Unknown",
        bundleID = meta.bundleID,
        pid = meta.pid or 0,
        spaceId = meta.spaceId,
        spaceIdx = meta.spaceIdx or 1,
        isMinimized = meta.isMinimized or nil,
        spaceGuessed = meta.spaceGuessed or nil,
      }
    end
    pcall(hs.settings.set, "hammerspoonWindowCache", serializableMeta)
    pcall(hs.settings.set, "hammerspoonMruIds", mruWindowIds)
  end)
end

local function refreshSpaceIndexCache()
  local spaceIndexById = {}
  local mainScr = hs.screen.mainScreen()
  local mainSpaces = getOrderedUserSpacesForCurrentScreen(mainScr)
  for idx, spId in ipairs(mainSpaces) do
    spaceIndexById[spId] = idx
  end
  for _, scr in ipairs(hs.screen.allScreens()) do
    if scr ~= mainScr then
      local scrSpaces = getOrderedUserSpacesForCurrentScreen(scr)
      for idx, spId in ipairs(scrSpaces) do
        if not spaceIndexById[spId] then
          spaceIndexById[spId] = idx
        end
      end
    end
  end
  cachedSpaceIndexById = spaceIndexById
  cachedCurrentSpaceId = hs.spaces.focusedSpace()
  cachedCurrentSpaceIdx = (cachedCurrentSpaceId and spaceIndexById[cachedCurrentSpaceId]) or 1
  return cachedSpaceIndexById, cachedCurrentSpaceId, cachedCurrentSpaceIdx
end

-- The focused Space and its workspace number. The Space layout (~15ms) is only
-- re-read when the focused Space changed since the last refresh: <Alt>Tab or a
-- focus event can arrive before the background sync that follows a Desktop
-- switch, and a stale workspace would mislabel the windows on screen and prune
-- the cached ones of the workspace just left.
local function currentSpaceInfo()
  if hs.spaces.focusedSpace() ~= cachedCurrentSpaceId then
    refreshSpaceIndexCache()
  end
  return cachedCurrentSpaceId, cachedCurrentSpaceIdx
end

local function recordActiveWindowFast(win, spaceId, spaceIdx)
  if not win then return nil end
  local wid = win:id()
  if not wid or wid <= 0 then return nil end

  local app = win:application()
  local appName = app and app:name() or "Unknown"
  if IGNORED_APP_NAMES[appName] then return nil end

  local bundleID = app and app:bundleID() or nil
  local pid = app and app:pid() or 0
  local title = win:title() or ""
  local frame = win:frame()

  if bundleID then getCachedAppIcon(bundleID) end

  local entry = knownWindowsById[wid] or {}
  entry.win = win
  entry.winId = wid
  entry.title = title
  entry.appName = appName
  entry.bundleID = bundleID
  entry.pid = pid
  entry.spaceId = spaceId or entry.spaceId or cachedCurrentSpaceId
  entry.spaceIdx = spaceIdx or entry.spaceIdx or cachedCurrentSpaceIdx
  entry.frame = frame
  entry.isMinimized = false  -- recordMinimizedWindows() sets both after recording
  entry.spaceGuessed = nil
  knownWindowsById[wid] = entry
  return wid
end

-- Called by Section 3 right after <Ctrl><Shift><Alt>Left/Right/1..6 moved a window,
-- so <Alt>Tab lists it under its new workspace immediately (before the async sync).
onWindowMovedToSpace = function(win, spaceId)
  local spaceIndexById = refreshSpaceIndexCache()
  local wid = recordActiveWindowFast(win, spaceId, spaceIndexById[spaceId])
  local entry = wid and knownWindowsById[wid]
  if entry then
    entry.isSticky = false
  end
  schedulePersistWindowTrackerState()
end

-- One Accessibility pass over the regular apps, returning (1) the visible windows
-- of the active Space front to back, exactly as hs.window.orderedWindows() does
-- (same per-app queries, same on-screen ordering), (2) the minimized windows,
-- which orderedWindows() leaves out, and (3) the set of pids of the apps it
-- skipped (see axGuard). AX reports minimized windows whichever Space they were
-- minimized on.
local getOrderedWindowIds = hs.window._orderedwinids  -- what orderedWindows() sorts by

-- Apps that do not answer an Accessibility query within axGuard.timeout (hung,
-- busy or starved of CPU) are skipped by the window passes for axGuard.skipSec;
-- their windows are kept as last seen but left out of <Alt>Tab meanwhile. A pass
-- runs on the main thread, and waiting out the default timeout (seconds per app)
-- stalled <Alt>Tab, every hotkey and the event taps -- a runaway Shortcuts at ~13
-- cores once made a pass take 7-8s. slow: pid -> time until which the app is
-- skipped. One table: the main chunk is close to Lua's 200 locals.
local axGuard = { timeout = 0.25, skipSec = 60, slow = {}, log = hs.logger.new("altTab", "info") }

-- How many windows app reports over Accessibility (asked with the short
-- timeout), or nil when it did not answer in time.
function axGuard.windowCount(app)
  local ok, el = pcall(hs.axuielement.applicationElement, app)
  if not ok or not el then return 0 end
  el:setTimeout(axGuard.timeout)
  local t0 = hs.timer.absoluteTime()
  local count = el:attributeValueCount("AXWindows")
  if count then return count end
  -- No count: no AXWindows at all (an answer), or no reply in time
  if (hs.timer.absoluteTime() - t0) / 1e9 >= axGuard.timeout * 0.8 then return nil end
  return 0
end

local function collectSwitcherWindows()
  local visibleById, minimized, skipped = {}, {}, {}
  local now = hs.timer.secondsSinceEpoch()
  for pid, untilT in pairs(axGuard.slow) do
    if untilT <= now then axGuard.slow[pid] = nil end
  end
  for _, app in ipairs(hs.application.runningApplications()) do
    if app:kind() > 0 then
      local pid = app:pid()
      local count = nil
      if not axGuard.slow[pid] then
        count = axGuard.windowCount(app)
        if not count then
          axGuard.slow[pid] = now + axGuard.skipSec
          axGuard.log.w(string.format("%s (pid %d) did not answer Accessibility within %.2fs; skipping it for %ds",
            tostring(app:name()), pid, axGuard.timeout, axGuard.skipSec))
        end
      end
      if not count then
        skipped[pid] = true
      elseif count > 0 then
        local hidden = app:isHidden()
        for _, w in ipairs(app:allWindows()) do
          if w:isMinimized() then
            table.insert(minimized, w)
          elseif not hidden then
            local id = w:id()
            if id then visibleById[id] = w end
          end
        end
      end
    end
  end
  if not getOrderedWindowIds then
    return hs.window.orderedWindows(), minimized, skipped
  end
  local ordered = {}
  for _, id in ipairs(getOrderedWindowIds()) do
    local w = visibleById[id]
    if w then table.insert(ordered, w) end
  end
  return ordered, minimized, skipped
end

-- Records the minimized windows and returns the set of their ids. A minimized
-- window belongs to the workspace WindowServer still places it on; when that is
-- several Spaces or none, to the one it was last seen on, and when that is unknown
-- too, to the current one (re-guessed on every pass until it is known).
local function recordMinimizedWindows(minimizedWins, focusedSpaceId)
  local ids = {}
  for _, w in ipairs(minimizedWins) do
    local wid = w:id()
    if wid and w:isStandard() then
      local okSp, winSpaces = pcall(hs.spaces.windowSpaces, wid)
      local userSpaces = {}
      for _, sp in ipairs((okSp and type(winSpaces) == "table") and winSpaces or {}) do
        if cachedSpaceIndexById[sp] then table.insert(userSpaces, sp) end
      end
      local spId, guessed = userSpaces[1], false
      if #userSpaces ~= 1 then
        local prev = knownWindowsById[wid]
        if prev and prev.spaceId and not prev.spaceGuessed and cachedSpaceIndexById[prev.spaceId] then
          spId = prev.spaceId
        else
          spId, guessed = focusedSpaceId, true
        end
      end
      wid = recordActiveWindowFast(w, spId, spId and cachedSpaceIndexById[spId])
      local entry = wid and knownWindowsById[wid]
      if entry then
        entry.isMinimized = true
        entry.spaceGuessed = guessed
        entry.isSticky = false
        ids[wid] = true
      end
    end
  end
  return ids
end

-- Background-only sync of windows across all Spaces (never run on <Alt>Tab critical path)
local function syncOtherSpacesWindowsAsync()
  local spaceIndexById, focusedSpaceId, currentSpaceIdx = refreshSpaceIndexCache()

  local orderedWins, minimizedWins, skippedPids = collectSwitcherWindows()
  local activeIds = {}
  for _, w in ipairs(orderedWins) do
    if w:isStandard() then
      local wid = recordActiveWindowFast(w, focusedSpaceId, currentSpaceIdx)
      if wid then activeIds[wid] = true end
    end
  end
  local minimizedIds = recordMinimizedWindows(minimizedWins, focusedSpaceId)

  -- Map Space window memberships in the background (without hs.window.list(true),
  -- which stalls the main CFRunLoop and causes WindowServer to disable CGEventTaps
  -- via kCGEventTapDisabledByTimeout).
  local spaceByWinId = {}
  local spaceCountByWinId = {}
  for spId, _ in pairs(spaceIndexById) do
    local okSp, winIds = pcall(hs.spaces.windowsForSpace, spId)
    if okSp and type(winIds) == "table" then
      for _, wid in ipairs(winIds) do
        spaceCountByWinId[wid] = (spaceCountByWinId[wid] or 0) + 1
        if spId == focusedSpaceId or not spaceByWinId[wid] then
          spaceByWinId[wid] = spId
        end
      end
    end
  end

  -- Prune closed windows and update Space indices
  for wid, meta in pairs(knownWindowsById) do
    local spId = spaceByWinId[wid]
    if minimizedIds[wid] then
      -- Workspace already attributed by recordMinimizedWindows()
    elseif activeIds[wid] then
      meta.spaceId = focusedSpaceId
      meta.spaceIdx = currentSpaceIdx
      meta.isSticky = (stickyWindows[wid] == true) or ((spaceCountByWinId[wid] or 0) > 1)
    elseif skippedPids[meta.pid] then
      -- Its app did not answer this pass (axGuard): keep it as last seen
    elseif spId then
      meta.spaceId = spId
      meta.spaceIdx = spaceIndexById[spId] or meta.spaceIdx or 1
      meta.isSticky = (stickyWindows[wid] == true) or ((spaceCountByWinId[wid] or 0) > 1)
      meta.isMinimized = false
    elseif meta.isMinimized then
      -- Minimized at the last look and now on no Space at all: it was closed
      knownWindowsById[wid] = nil
    else
      local appAlive = (meta.pid and meta.pid > 0 and hs.application.applicationForPID(meta.pid) ~= nil)
      if not appAlive or meta.spaceIdx == currentSpaceIdx then
        knownWindowsById[wid] = nil
      end
    end
  end

  schedulePersistWindowTrackerState()
end

local function scheduleBackgroundSpaceSync(delaySec)
  if bgSpaceSyncTimer then
    bgSpaceSyncTimer:stop()
  end
  bgSpaceSyncTimer = hs.timer.doAfter(delaySec or 0.15, function()
    bgSpaceSyncTimer = nil
    syncOtherSpacesWindowsAsync()
  end)
end

local wf = hs.window.filter.new()
wf:subscribe(hs.window.filter.windowFocused, function(win)
  if not win then return end
  local spaceId, spaceIdx = currentSpaceInfo()
  local wid = recordActiveWindowFast(win, spaceId, spaceIdx)
  if not wid then return end
  for i = #mruWindowIds, 1, -1 do
    if mruWindowIds[i] == wid then
      table.remove(mruWindowIds, i)
    end
  end
  table.insert(mruWindowIds, 1, wid)
  while #mruWindowIds > 64 do
    table.remove(mruWindowIds)
  end
  schedulePersistWindowTrackerState()
end)

wf:subscribe(hs.window.filter.windowDestroyed, function(win)
  if not win then return end
  local okId, id = pcall(function() return win:id() end)
  if not okId or not id then return end
  knownWindowsById[id] = nil
  for i = #mruWindowIds, 1, -1 do
    if mruWindowIds[i] == id then
      table.remove(mruWindowIds, i)
    end
  end
  schedulePersistWindowTrackerState()
end)

local mruSpaceWatcher = hs.spaces.watcher.new(function(_)
  scheduleBackgroundSpaceSync(0.1)
end)
mruSpaceWatcher:start()
_G.HS_KEEPALIVE.filters.wf = wf
_G.HS_KEEPALIVE.watchers.mruSpaceWatcher = mruSpaceWatcher
_G.HS_KEEPALIVE.taps.altDragTap = altDragTap
scheduleBackgroundSpaceSync(0.2)

-- Pre-allocate a single reusable hs.canvas border window (avoiding NSWindow
-- creation/destruction churn on every row navigation)
local function ensureHighlightCanvas()
  if highlightCanvas then return highlightCanvas end
  highlightCanvas = hs.canvas.new({x = 0, y = 0, w = 100, h = 100})
  highlightCanvas:appendElements({
    type = "rectangle",
    action = "stroke",
    strokeWidth = 10,
    strokeColor = {red = 0.0, green = 0.0, blue = 1.0, alpha = 0.85},
    roundedRectRadii = {xRadius = 4, yRadius = 4},
    frame = {x = 5, y = 5, w = 90, h = 90},
  })
  highlightCanvas:level(hs.canvas.windowLevels.overlay)
  return highlightCanvas
end

local function hideWindowBorder()
  if borderDebounceTimer then
    borderDebounceTimer:stop()
    borderDebounceTimer = nil
  end
  if highlightCanvas then
    highlightCanvas:hide()
  end
end

local function clearWindowBorder()
  if chooserPollTimer then
    chooserPollTimer:stop()
    chooserPollTimer = nil
  end
  hideWindowBorder()
  lastSelectedRow = nil
end

local function drawBlueBorderAroundFrame(f)
  if not f or f.w <= 0 or f.h <= 0 then
    if highlightCanvas then highlightCanvas:hide() end
    return
  end
  local pad = 10
  local canvas = ensureHighlightCanvas()
  canvas:frame({
    x = f.x - pad,
    y = f.y - pad,
    w = f.w + (pad * 2),
    h = f.h + (pad * 2),
  })
  canvas[1].frame = {x = 5, y = 5, w = f.w + 10, h = f.h + 10}
  canvas:show()
end

-- Asynchronous, debounced border updater (throttles rapid Up/Down or typing)
local function scheduleBorderUpdate()
  if borderDebounceTimer then
    borderDebounceTimer:stop()
  end
  borderDebounceTimer = hs.timer.doAfter(BORDER_DEBOUNCE_SEC, function()
    borderDebounceTimer = nil
    if not windowChooser or not windowChooser:isVisible() then
      if highlightCanvas then highlightCanvas:hide() end
      return
    end
    local row = windowChooser:selectedRow()
    local item = row and windowChooser:selectedRowContents(row) or nil
    if item and item.onCurrentSpace and item.frame then
      drawBlueBorderAroundFrame(item.frame)
    else
      if highlightCanvas then highlightCanvas:hide() end
    end
  end)
end

-- Fast choice builder called on <Alt>Tab: one Accessibility pass for the active
-- Space's windows (front to back) and all minimized windows, merged with cached
-- windows from other workspaces.
local function buildAllWorkspacesChooserChoices()
  local focusedSpaceId, currentSpaceIdx = currentSpaceInfo()
  currentSpaceIdx = currentSpaceIdx or 1

  local activeOrdered, minimizedWins, skippedPids = collectSwitcherWindows()
  local activeSet = {}
  for _, w in ipairs(activeOrdered) do
    if w:isStandard() then
      local wid = recordActiveWindowFast(w, focusedSpaceId, currentSpaceIdx)
      if wid then activeSet[wid] = true end
    end
  end
  local minimizedSet = recordMinimizedWindows(minimizedWins, focusedSpaceId)

  local validEntriesById = {}
  local candidateIds = {}
  for wid, meta in pairs(knownWindowsById) do
    local onActive = (activeSet[wid] == true)
    local isMinimized = (minimizedSet[wid] == true)
    local lastSeen = (skippedPids[meta.pid] == true)
    if meta.isMinimized and not isMinimized and not onActive and not lastSeen then
      -- Minimized at the last look but no longer: restored onto another Space
      -- (keep it there) or closed (no Space at all: pruned just below)
      local okSp, winSpaces = pcall(hs.spaces.windowSpaces, wid)
      local spId = nil
      for _, sp in ipairs((okSp and type(winSpaces) == "table") and winSpaces or {}) do
        if cachedSpaceIndexById[sp] then spId = sp break end
      end
      meta.isMinimized = false
      meta.spaceId = spId
      meta.spaceIdx = spId and cachedSpaceIndexById[spId] or nil
    end
    if lastSeen then
      -- Its app did not answer this pass (axGuard): keep the window as last seen,
      -- but leave it out of the list -- focusing it would block on the app too
    elseif onActive or isMinimized or (meta.spaceIdx and meta.spaceIdx ~= currentSpaceIdx) then
      local isSticky = not isMinimized and ((stickyWindows[wid] == true) or (meta.isSticky == true))
      local rawSpaceIdx = onActive and currentSpaceIdx or (meta.spaceIdx or 1)
      local displaySpaceIdx = isSticky and currentSpaceIdx or rawSpaceIdx
      meta.spaceIdx = rawSpaceIdx
      meta.displaySpaceIdx = displaySpaceIdx
      meta.isSticky = isSticky
      meta.onCurrentSpace = onActive or isSticky
      validEntriesById[wid] = meta
      table.insert(candidateIds, wid)
    else
      knownWindowsById[wid] = nil
    end
  end

  -- Place PREV_WINDOW_ID on current workspace first so <Alt>Tab -> Enter
  -- immediately toggles between the two most recently focused windows.
  local focusedWinId = (activeOrdered[1] and activeOrdered[1]:id()) or mruWindowIds[1]
  local prevOnCurrentSpaceId = nil
  for _, id in ipairs(mruWindowIds) do
    local entry = validEntriesById[id]
    if entry and id ~= focusedWinId and entry.displaySpaceIdx == currentSpaceIdx and not entry.isMinimized then
      prevOnCurrentSpaceId = id
      break
    end
  end
  if not prevOnCurrentSpaceId and activeOrdered[2] then
    local wid = activeOrdered[2]:id()
    if wid and validEntriesById[wid] then
      prevOnCurrentSpaceId = wid
    end
  end

  local seen = {}
  local orderedIds = {}
  local function pushId(id, allowMinimized)
    local entry = id and validEntriesById[id]
    if entry and not seen[id] and (allowMinimized or not entry.isMinimized) then
      table.insert(orderedIds, id)
      seen[id] = true
    end
  end

  pushId(prevOnCurrentSpaceId)
  if mruWindowIds[2] and mruWindowIds[2] ~= focusedWinId then
    pushId(mruWindowIds[2])
  end
  for _, id in ipairs(mruWindowIds) do
    pushId(id)
  end
  for _, w in ipairs(activeOrdered) do
    pushId(w:id())
  end
  table.sort(candidateIds, function(a, b)
    local ea, eb = validEntriesById[a], validEntriesById[b]
    if ea.displaySpaceIdx ~= eb.displaySpaceIdx then
      return ea.displaySpaceIdx < eb.displaySpaceIdx
    end
    return a < b
  end)
  for _, id in ipairs(candidateIds) do
    pushId(id)
  end
  -- Minimized windows go last (most recently used first), so restoring one is
  -- always a deliberate choice rather than the default <Alt>Tab -> Enter.
  for _, id in ipairs(mruWindowIds) do
    pushId(id, true)
  end
  for _, id in ipairs(candidateIds) do
    pushId(id, true)
  end

  local choices = {}
  for _, wid in ipairs(orderedIds) do
    local meta = validEntriesById[wid]
    local appName = meta.appName or "Unknown"
    local title = (meta.title and meta.title ~= "") and meta.title or appName
    local shortPrefix = normalizeShortAppPrefix(appName)
    local prefixedTitle = title
    if shortPrefix then
      local lowerTitle = string.lower(title)
      if not lowerTitle:find("^" .. shortPrefix .. ":") then
        prefixedTitle = shortPrefix .. ": " .. title
      end
    else
      local lowerTitle = string.lower(title)
      local lowerApp = string.lower(appName)
      if not lowerTitle:find(lowerApp, 1, true) then
        prefixedTitle = appName .. ": " .. title
      end
    end

    local entryText = string.format("%d %s (0x%x)", meta.displaySpaceIdx, prefixedTitle, wid)
    local stickyTag = meta.isSticky and " [All Spaces]" or ""
    local minimizedTag = meta.isMinimized and " [Minimized]" or ""
    local subText = string.format(
      "Workspace %d%s%s | %s | PID: %d | Window ID: %d",
      meta.displaySpaceIdx, stickyTag, minimizedTag, appName, meta.pid or 0, wid
    )
    local textLower = string.lower(entryText)
    local searchText = textLower .. " " .. string.lower(subText)

    table.insert(choices, {
      text = entryText,
      subText = subText,
      image = getCachedAppIcon(meta.bundleID),
      winId = wid,
      spaceId = meta.spaceId,
      spaceIdx = meta.spaceIdx,
      displaySpaceIdx = meta.displaySpaceIdx,
      isSticky = meta.isSticky,
      isMinimized = meta.isMinimized,
      onCurrentSpace = meta.onCurrentSpace,
      frame = meta.frame,
      bundleID = meta.bundleID,
      appName = appName,
      textLower = textLower,
      searchText = searchText,
    })
  end

  return choices, currentSpaceIdx, activeOrdered
end

local function choiceMatchesQuery(choice, rawQuery)
  if not rawQuery or rawQuery == "" or rawQuery == "^" then
    return true
  end

  -- If the user pressed Backspace once on "^1 " so it became "^1" (no trailing
  -- space), immediately show all windows from all workspaces!
  if rawQuery:match("^%^[0-9]+$") then
    return true
  end

  local q = rawQuery
  -- Require trailing whitespace in "^<workspace_num> " to filter by workspace
  local wsPrefix, remainder = q:match("^%^([0-9]+)%s+(.*)")
  if wsPrefix then
    local targetWs = tonumber(wsPrefix)
    if choice.displaySpaceIdx ~= targetWs then
      return false
    end
    q = remainder or ""
  else
    -- If user deleted the space ("^1") and typed search text ("^1chrome"),
    -- strip the inactive "^1" prefix and filter across all workspaces.
    q = q:gsub("^%^[0-9]+", "")
  end

  q = q:match("^%s*(.-)%s*$") or ""
  if q == "" or q == "^" then
    return true
  end

  local haystack = choice.searchText or string.lower((choice.text or "") .. " " .. (choice.subText or ""))
  local textLower = choice.textLower or string.lower(choice.text or "")

  for token in q:gmatch("%S+") do
    local tokLower = string.lower(token)
    if tokLower:sub(1, 1) == "^" then
      local anchored = tokLower:sub(2)
      if anchored ~= "" and textLower:sub(1, #anchored) ~= anchored then
        return false
      end
    else
      if not haystack:find(tokLower, 1, true) then
        local ok, matched = pcall(function() return haystack:match(tokLower) ~= nil end)
        if not (ok and matched) then
          return false
        end
      end
    end
  end
  return true
end

local function filterChooserChoices(choices, rawQuery)
  local filtered = {}
  for _, c in ipairs(choices) do
    if choiceMatchesQuery(c, rawQuery) then
      table.insert(filtered, c)
    end
  end
  return filtered
end

-- Cancels a pending focus hand-off (it must never override a newer <Alt>Tab).
local function cancelSwitcherFocus()
  switcherFocusToken = switcherFocusToken + 1
  if switcherFocusTimer then
    switcherFocusTimer:stop()
    switcherFocusTimer = nil
  end
end

-- Runs fn after sec seconds unless the hand-off was cancelled meanwhile, holding a
-- strong reference to the timer (an unreferenced hs.timer is garbage-collected
-- and silently never fires).
local function afterSwitcherFocus(token, sec, fn)
  if switcherFocusTimer then switcherFocusTimer:stop() end
  switcherFocusTimer = hs.timer.doAfter(sec, function()
    switcherFocusTimer = nil
    if token == switcherFocusToken then fn() end
  end)
end

-- Set of the Space ids currently shown on any display.
local function visibleSpaceSet()
  local set = {}
  local ok, active = pcall(hs.spaces.activeSpaces)
  for _, sp in pairs((ok and type(active) == "table") and active or {}) do
    set[sp] = true
  end
  return set
end

-- The hs.window for winId: the tracked object (it stays valid while its Space is
-- not visible), else a fresh lookup.
local function resolveSwitcherWindow(winId)
  local meta = knownWindowsById[winId]
  local w = meta and meta.win
  if w then
    local ok, id = pcall(function() return w:id() end)
    if ok and id == winId then return w end
  end
  return hs.window.get(winId)
end

-- Focuses exactly this window. Never hs.application:activate() first: it brings
-- the app's *own* focused window forward (app:focusedWindow():becomeMain() +
-- _bringtofront()), and when that window is on another Space macOS follows it
-- there ("When switching to an application, switch to a Space with open windows
-- for the application") -- picking a Chrome window on this workspace used to jump
-- to the Chrome window last used on another one. hs.window:focus() makes *this*
-- window main before bringing its app forward with only its front window.
local function focusWindowExactly(w)
  return pcall(function()
    if w:isMinimized() then w:unminimize() end
    w:raise()
    w:focus()
  end)
end

-- Focuses winId now and re-checks a few times while macOS settles (an unminimize
-- animation or a just-finished Space switch can swallow a single attempt).
local SWITCHER_FOCUS_RETRIES = 3
local SWITCHER_FOCUS_RETRY_SEC = 0.15

local function focusSwitcherWindow(token, winId)
  local retries = 0
  local function attempt()
    local w = resolveSwitcherWindow(winId)
    if not w then return end
    focusWindowExactly(w)
    if retries >= SWITCHER_FOCUS_RETRIES then return end
    retries = retries + 1
    afterSwitcherFocus(token, SWITCHER_FOCUS_RETRY_SEC, function()
      local fw = hs.window.focusedWindow()
      if not (fw and fw:id() == winId) then attempt() end
    end)
  end
  attempt()
end

local function focusSelectedWindowChoice(choice)
  if not choice or not choice.winId then return end
  cancelSwitcherFocus()
  local token = switcherFocusToken
  local winId = choice.winId
  local targetSpaceId = choice.spaceId

  -- On screen here, on every workspace (sticky), or a minimized window of this
  -- workspace (or of an unknown one): focus it right away
  if choice.onCurrentSpace or not targetSpaceId or visibleSpaceSet()[targetSpaceId] then
    focusSwitcherWindow(token, winId)
    return
  end

  -- Different workspace: switch to the target workspace first, then raise & focus
  -- window (looked up by Space id, as workspace numbers shift when Spaces change)
  local targetIdx = indexOf(getOrderedUserSpacesForCurrentScreen(hs.screen.mainScreen()), targetSpaceId)
  if not targetIdx then
    -- Not a workspace of this display (anymore): focusing it lets macOS follow it
    focusSwitcherWindow(token, winId)
    return
  end
  switchToWorkspaceIndex(targetIdx)
  local deadline = hs.timer.secondsSinceEpoch() + 3.0
  local function focusOnArrival()
    if hs.spaces.focusedSpace() == targetSpaceId or hs.timer.secondsSinceEpoch() >= deadline then
      focusSwitcherWindow(token, winId)
    else
      afterSwitcherFocus(token, 0.05, focusOnArrival)
    end
  end
  afterSwitcherFocus(token, 0.05, focusOnArrival)
end

-- <Escape>: give focus back to the window that had it when <Alt>Tab opened the
-- chooser (a non-activating panel that took keyboard focus) -- unless that window
-- is not on a visible Space (an app keeps its focused window after you switch
-- Desktops away from it), in which case the frontmost window here gets focus
-- instead, so dismissing the switcher never switches Desktops. Nothing is
-- refocused when the chooser closed because another window was clicked.
local function restorePreChooserFocus()
  local w = preChooserFocusedWindow
  preChooserFocusedWindow = nil
  local fw = hs.window.focusedWindow()
  local fwApp = fw and fw:application()
  if fwApp and fwApp:name() ~= "Hammerspoon" and not (w and fw:id() == w:id()) then
    return
  end
  if w then
    local okSp, winSpaces = pcall(function() return hs.spaces.windowSpaces(w:id()) end)
    local visible = visibleSpaceSet()
    for _, sp in ipairs((okSp and type(winSpaces) == "table") and winSpaces or {}) do
      if visible[sp] then
        pcall(function() w:focus() end)
        return
      end
    end
  end
  for _, ow in ipairs(hs.window.orderedWindows()) do
    local app = ow:application()
    if app and app:name() ~= "Hammerspoon" and ow:isStandard() then
      pcall(function() ow:focus() end)
      return
    end
  end
end

windowChooser = hs.chooser.new(function(choice)
  clearWindowBorder()
  if choice then
    preChooserFocusedWindow = nil
    focusSelectedWindowChoice(choice)
  else
    restorePreChooserFocus()
  end
end)
windowChooser:bgDark(true)
windowChooser:searchSubText(true)

-- hs.chooser's default globalCallback refocuses the window that had focus when a
-- chooser opened on "didClose", which fires *before* the completion callback
-- above -- so choosing a window first re-activated the previous one, and when
-- that one sits on another Space, macOS switched Desktops there. The switcher
-- hands focus over itself (see above); every other chooser keeps the default.
hs.chooser.globalCallback = function(whichChooser, state)
  if whichChooser == windowChooser then return end
  return hs.chooser._defaultGlobalCallback(whichChooser, state)
end

windowChooser:queryChangedCallback(function(newQuery)
  local filtered = filterChooserChoices(allChooserChoices, newQuery)
  windowChooser:choices(filtered)
  lastSelectedRow = nil
  scheduleBorderUpdate()
end)

local function getFocusedWindowScreen(activeOrdered)
  local fw = hs.window.focusedWindow()
  if fw then
    local app = fw:application()
    if not (app and app:name() == "Hammerspoon") then
      local s = fw:screen()
      if s then return s end
    end
  end
  if type(activeOrdered) == "table" and activeOrdered[1] then
    local s = activeOrdered[1]:screen()
    if s then return s end
  end
  return hs.screen.mainScreen() or hs.screen.primaryScreen()
end

local function openWindowSwitcherChooser()
  cancelSwitcherFocus()
  if not windowChooser:isVisible() then
    local fw = hs.window.focusedWindow()
    local fwApp = fw and fw:application()
    preChooserFocusedWindow = (fwApp and fwApp:name() ~= "Hammerspoon") and fw or nil
  end
  local choices, currentSpaceIdx, activeOrdered = buildAllWorkspacesChooserChoices()
  local targetScreen = getFocusedWindowScreen(activeOrdered)
  allChooserChoices = choices
  initialChooserQuery = string.format("^%d ", currentSpaceIdx)

  windowChooser:choices(filterChooserChoices(allChooserChoices, initialChooserQuery))
  windowChooser:query(initialChooserQuery)
  if targetScreen then
    local sf = targetScreen:frame()
    local widthPct = windowChooser:width()
    local chooserW = math.floor(sf.w * (((widthPct and widthPct > 0) and widthPct or 40) / 100.0))
    windowChooser:show({
      x = math.floor(sf.x + (sf.w - chooserW) / 2),
      y = math.floor(sf.y + sf.h * 0.20),
    })
  else
    windowChooser:show()
  end
  scheduleBorderUpdate()

  lastSelectedRow = nil
  if chooserPollTimer then chooserPollTimer:stop() end
  chooserPollTimer = hs.timer.doEvery(0.05, function()
    if not windowChooser:isVisible() then
      clearWindowBorder()
      return
    end
    local row = windowChooser:selectedRow()
    if row ~= lastSelectedRow then
      lastSelectedRow = row
      scheduleBorderUpdate()
    end
  end)
end

_G.HS_KEEPALIVE.choosers.windowChooser = windowChooser
keepHotkey({"alt"}, "tab", openWindowSwitcherChooser)

-- Debug hook for the live Hammerspoon CLI: the rows <Alt>Tab would list right
-- now (all workspaces, unfiltered), without showing the chooser, e.g.
--   hs -c 'return hs.inspect(windowSwitcher.rows())'
-- axGuard, collect (the window pass) and known (a tracked window's record) are
-- exposed for live tests.
windowSwitcher = {
  rows = function()
    local rows = {}
    for i, c in ipairs((buildAllWorkspacesChooserChoices())) do
      rows[i] = c.text .. "  |  " .. c.subText
    end
    return rows
  end,
  axGuard = axGuard,
  collect = collectSwitcherWindows,
  known = function(wid) return knownWindowsById[wid] end,
}

-- High-priority CGEventTap for <Alt>Tab so macOS Spotlight or symbolic hotkeys
-- registered at login can never hijack Option+Tab before Hammerspoon sees it.
-- Stored in _G.HS_KEEPALIVE.taps.altTabTap (and monitored by tapWatchdog) so
-- neither Lua 5.4 garbage collection nor macOS kCGEventTapDisabledByTimeout
-- can ever stop it after a few seconds.
local TAB_KEYCODE = (hs.keycodes and hs.keycodes.map and hs.keycodes.map["tab"]) or 48
local altTabOpenTimer = nil
local altTabTap = nil
altTabTap = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(e)
  if e:getKeyCode() ~= TAB_KEYCODE then return false end
  local flags = e:getFlags()
  if flags.alt and not flags.cmd and not flags.ctrl and not flags.shift then
    if altTabOpenTimer then altTabOpenTimer:stop() end
    altTabOpenTimer = hs.timer.doAfter(0, function()
      altTabOpenTimer = nil
      openWindowSwitcherChooser()
    end)
    _G.HS_KEEPALIVE.timers.altTabOpenTimer = altTabOpenTimer
    return true
  end
  return false
end)
altTabTap:start()
_G.HS_KEEPALIVE.taps.altTabTap = altTabTap

-- Watchdog that re-enables altTabTap and altDragTap if macOS WindowServer ever
-- disables a CGEventTap due to kCGEventTapDisabledByTimeout.
local tapWatchdog = nil
tapWatchdog = hs.timer.doEvery(2.0, function()
  if altTabTap and not altTabTap:isEnabled() then
    altTabTap:start()
  end
  if altDragTap and not altDragTap:isEnabled() and not (spaceMove and spaceMove.op) then
    altDragTap:start()
  end
  if switchShortcutTap and not switchShortcutTap:isEnabled() then
    switchShortcutTap:start()
  end
end)
_G.HS_KEEPALIVE.timers.tapWatchdog = tapWatchdog

-- -----------------------------------------------------------------------------
-- 7. Zen Mode (<WM_MOD>z), Terminal-Scoped Notes (<WM_MOD>n), & App Shortcuts
-- -----------------------------------------------------------------------------
keepHotkey(WM_MOD, "z", function()
  hs.task.new("/bin/bash", nil, {os.getenv("HOME") .. "/scripts/zen-mode.sh"}):start()
end)

-- Terminal-scoped Notes launcher (replicates `if-terminal-has-focus ...`)
keepHotkey(WM_MOD, "n", function()
  local frontApp = hs.application.frontmostApplication()
  local name = frontApp and string.lower(frontApp:name() or "") or ""
  if name == "kitty" or name == "wezterm" or name == "iterm2" or name == "terminal" then
    hs.task.new("/bin/bash", nil, {"-lc", os.getenv("HOME") .. "/.googlerc.d/bin/kitty-ssh-launch tab"}):start()
  end
end)

-- Launchers matching xfce4-keyboard-shortcuts.xml
keepHotkey({"ctrl", "alt"}, "t", function()
  hs.application.launchOrFocus("kitty")
end)

keepHotkey({"ctrl", "alt"}, "l", function()
  hs.caffeinate.lockScreen()
end)

-- -----------------------------------------------------------------------------
-- 8. Monitor Re-sync After Display Wake (Elgato Dock DisplayPort; <WM_MOD>r),
--    Warm-up Brightness/Contrast & Steps (<WM_MOD>6/7, <WM_MOD>F6/F7)
-- -----------------------------------------------------------------------------
-- After the displays wake (already on the lock screen), the CR240DM plugged into
-- the Elgato dock's DisplayPort output reconnects and then, about 20s later,
-- can flicker and stay blank while macOS still has it connected. Switching its
-- refresh rate to another rate and back (a fresh mode set) brings the picture
-- back, so do that automatically, but only when needed: each time that monitor
-- comes back after dropping out since the displays slept (it does once the
-- monitors reached "no signal" standby, not after a short lock), shortly after
-- the wake. <WM_MOD>r runs the same switch on demand, for when the monitor stays
-- blank anyway (after hours off, a switch may only stick once it has been on for
-- a few minutes) or goes blank later; macOS still reports it as working then,
-- so that can't be detected.
--
-- Only acts on a CR240DM that reaches the Mac through the Elgato dock's own
-- DisplayPort output, wherever it sits in the arrangement: never on one that
-- macOS reports behind a DisplayPort branch device (the KVM's hub or an
-- adapter). Monitors are matched to the dock's DisplayPort tunnels by EDID
-- serial number. macOS refreshes those tunnel details only on some link events,
-- so when they don't mention a monitor, the one CR240DM that is not behind a
-- branch device is taken to be on the dock's DisplayPort (none if ambiguous).
-- A monitor last seen behind a branch device (remembered per screen UUID, also
-- across reloads) never counts as that one: with the left monitor off at a
-- wake, the details went stale and the KVM monitor was re-synced instead.
local DISPLAY_RESYNC = {
  monitorName = "CR240DM",        -- monitor model (EDID name); nil for any model
  dockVendor = "Elgato Systems",  -- Thunderbolt dock vendor reported by macOS
  settleSec = 2,    -- quiet period after the monitor reconnects before acting
  windowSec = 45,   -- only act on reconnects this long after a wake
  holdSec = 0.3,    -- time on the alternate refresh rate before switching back
  maxPerWake = 3,   -- re-sync at most this many times per wake
}
local resyncLog = hs.logger.new("dispResync", "info")
local resyncState = {
  awake = true, wakeAt = 0, count = 0, busyUntil = 0, checking = false,
  timer = nil, restoreTimers = {}, tasks = {},
  tracked = {},      -- screen UUIDs present (or dropped out) at the last check
  missing = {},      -- screen UUIDs that dropped out since the displays slept
  reconnected = {},  -- screen UUIDs that came back after dropping out
  -- screen UUID -> "direct" / "branch": how the dock's tunnel details last
  -- showed that monitor (persisted, see eligibleScreens)
  links = hs.settings.get("dispResync.dockLinks") or {},
}

-- Returns the current mode and another refresh rate at the same resolution:
-- 60 Hz when the display is not already at 60 Hz, otherwise the closest other
-- rate (the higher one on ties). The second value is nil if there is none.
local function alternateMode(scr)
  local cur = scr:currentMode()
  if not cur then return nil, nil end
  local best = nil
  for _, m in pairs(scr:availableModes() or {}) do
    if m.w == cur.w and m.h == cur.h and m.scale == cur.scale and math.abs(m.freq - cur.freq) >= 0.5 then
      if math.abs(cur.freq - 60) >= 0.5 and math.abs(m.freq - 60) < 0.5 then
        return cur, m
      end
      local dm = math.abs(m.freq - cur.freq)
      local db = best and math.abs(best.freq - cur.freq) or math.huge
      if dm < db or (dm == db and m.freq > best.freq) then best = m end
    end
  end
  return cur, best
end

-- Parses `system_profiler -json SPDisplaysDataType` into display ID -> EDID
-- serial number (system_profiler prints it in hex and omits it when it is 0).
local function displaySerials(json)
  local serials = {}
  local ok, data = pcall(hs.json.decode, json or "")
  for _, gpu in ipairs(ok and type(data) == "table" and data.SPDisplaysDataType or {}) do
    for _, d in ipairs(gpu.spdisplays_ndrvs or {}) do
      if d._spdisplays_displayID then
        serials[tostring(d._spdisplays_displayID)] = tonumber(d["_spdisplays_display-serial-number"] or "0", 16) or 0
      end
    end
  end
  return serials
end

-- Parses `ioreg -a -l -r -c IOPortTransportStateCIO -d 2` into the DisplayPort
-- sinks tunneled through the given dock vendor's Thunderbolt connection:
-- { present = bool, sinks = { { product, serial, direct } } }, where direct
-- means no branch device (DP-to-HDMI converter or MST hub such as the KVM).
local function dockDisplayPortSinks(xml, vendor)
  local dock = { present = false, sinks = {} }
  local ok, nodes = pcall(hs.plist.readString, xml or "")
  for _, node in ipairs(ok and type(nodes) == "table" and nodes or {}) do
    if node["Device Vendor Name"] == vendor then
      dock.present = true
      for _, child in ipairs(node.IORegistryEntryChildren or {}) do
        if child.IOObjectClass == "IOPortTransportStateDisplayPort" and child.ProductName then
          table.insert(dock.sinks, {
            product = child.ProductName,
            serial = tonumber(child.SerialNumber) or 0,
            direct = child.BranchDeviceOUI == nil and child.BranchDeviceID == nil,
          })
        end
      end
    end
  end
  return dock
end

-- Screen name without the " (2)" suffix macOS adds to identical monitors.
local function modelName(scr)
  return ((scr:name() or ""):gsub(" %(%d+%)$", ""))
end

-- Returns the screens on the dock's DisplayPort output that qualify for a
-- re-sync (possibly none) and the reason. With `links` (screen UUID ->
-- "direct" / "branch"), records how the tunnel details show each monitor, and
-- a monitor they don't mention that was last seen behind a branch device is
-- not taken for the one on the dock's DisplayPort.
local function eligibleScreens(screens, serials, dock, links)
  local model = DISPLAY_RESYNC.monitorName
  if not (dock and dock.present) then
    return {}, DISPLAY_RESYNC.dockVendor .. " dock not attached"
  end
  local direct, unknown, candidates, remembered = {}, {}, 0, 0
  for _, scr in ipairs(screens or {}) do
    local name = modelName(scr)
    if not name:find("^Built%-in") and (model == nil or name == model) then
      candidates = candidates + 1
      local serial = (serials or {})[tostring(scr:id())] or 0
      local uuid = links and scr.getUUID and scr:getUUID()
      local onDirect, onBranch = false, false
      for _, sink in ipairs(dock.sinks or {}) do
        if sink.product == name and sink.serial == serial then
          if sink.direct then onDirect = true else onBranch = true end
        end
      end
      if onDirect then
        direct[#direct + 1] = scr
        if uuid then links[uuid] = "direct" end
      elseif onBranch then
        if uuid then links[uuid] = "branch" end
      elseif uuid and links[uuid] == "branch" then
        remembered = remembered + 1
      else
        unknown[#unknown + 1] = scr
      end
    end
  end
  local what = model or "external monitor"
  if #direct > 0 then return direct, "on the dock's DisplayPort" end
  if #unknown == 1 then return unknown, "the only " .. what .. " not behind the KVM" end
  if #unknown > 1 then return {}, "can't tell which " .. what .. " is on the dock's DisplayPort" end
  if remembered > 0 then return {}, what .. " was last seen behind the KVM" end
  if candidates > 0 then return {}, what .. " reaches the dock through a KVM or adapter" end
  return {}, "no " .. what .. " connected"
end

local function findScreenByUUID(uuid)
  for _, scr in ipairs(hs.screen.allScreens()) do
    if scr:getUUID() == uuid then return scr end
  end
  return nil
end

local function currentScreenUUIDs()
  local present = {}
  for _, scr in ipairs(hs.screen.allScreens()) do
    -- A screen can have no UUID yet while the displays wake (present[nil] used
    -- to raise "table index is nil" and abort the wake handling)
    local uuid = scr:getUUID()
    if uuid then present[uuid] = true end
  end
  return present
end

-- Updates the drop-out tracking in `state` with the current screen UUIDs and
-- returns true when a screen that dropped out since the displays slept is back
-- (its UUID is then added to state.reconnected). While `busy` (our own mode
-- switches) drop-outs are not recorded, so they can never trigger a re-sync.
local function trackScreens(state, current, busy)
  local back = false
  if not busy then
    for uuid in pairs(state.tracked) do
      if not current[uuid] then state.missing[uuid] = true end
    end
    for uuid in pairs(current) do
      if state.missing[uuid] then
        state.missing[uuid] = nil
        state.reconnected[uuid] = true
        back = true
      end
    end
  end
  state.tracked = {}
  for uuid in pairs(current) do state.tracked[uuid] = true end
  if not busy then
    for uuid in pairs(state.missing) do state.tracked[uuid] = true end
  end
  return back
end

-- Switches the screen to an alternate refresh rate and back.
local function resyncScreen(scr, reason, basis)
  local cur, alt = alternateMode(scr)
  if not (cur and alt) then
    resyncLog.i(reason .. ": no other refresh rate for " .. tostring(scr:name()))
    return false
  end
  local uuid = scr:getUUID()
  -- Ignore the display changes caused by our own mode switches.
  resyncState.busyUntil = hs.timer.secondsSinceEpoch() + DISPLAY_RESYNC.holdSec + 4
  resyncLog.i(string.format("%s: %s (%s) %dx%d@%gHz -> %gHz and back",
    reason, tostring(scr:name()), basis, cur.w, cur.h, cur.freq, alt.freq))
  scr:setMode(alt.w, alt.h, alt.scale, alt.freq, alt.depth)
  resyncState.restoreTimers[uuid] = hs.timer.doAfter(DISPLAY_RESYNC.holdSec, function()
    resyncState.restoreTimers[uuid] = nil
    local s = findScreenByUUID(uuid)
    if s then s:setMode(cur.w, cur.h, cur.scale, cur.freq, cur.depth) end
    resyncState.busyUntil = hs.timer.secondsSinceEpoch() + 3
  end)
  return true
end

-- Collects display serials and the dock's DisplayPort sinks in the background
-- (system_profiler and ioreg), then re-syncs the qualifying monitors; with
-- requireReconnect, only those that came back after dropping out. With
-- learnOnly it just records (and saves) how each monitor reaches the dock.
local function resyncIfEligible(reason, onDone, requireReconnect, learnOnly)
  if resyncState.checking then return end
  resyncState.checking = true
  local facts, pending = {}, 2
  local function collected()
    pending = pending - 1
    if pending > 0 then return end
    resyncState.checking, resyncState.tasks = false, {}
    local targets, why = eligibleScreens(hs.screen.allScreens(), facts.serials, facts.dock, resyncState.links)
    hs.settings.set("dispResync.dockLinks", resyncState.links)
    if learnOnly then return end
    local count = 0
    for _, scr in ipairs(targets) do
      local uuid = scr:getUUID()
      if not requireReconnect or resyncState.reconnected[uuid] then
        resyncState.reconnected[uuid] = nil
        if resyncScreen(scr, reason, why) then count = count + 1 end
      else
        resyncLog.i(reason .. ": skipped " .. tostring(scr:name()) .. " (it did not drop out after the displays slept)")
      end
    end
    if #targets == 0 then resyncLog.i(reason .. ": skipped (" .. why .. ")") end
    if onDone then onDone(count, why, targets) end
  end
  local profiler = hs.task.new("/usr/sbin/system_profiler", function(_, out)
    facts.serials = displaySerials(out)
    collected()
  end, { "-json", "SPDisplaysDataType" })
  local registry = hs.task.new("/usr/sbin/ioreg", function(_, out)
    facts.dock = dockDisplayPortSinks(out, DISPLAY_RESYNC.dockVendor)
    collected()
  end, { "-a", "-l", "-r", "-c", "IOPortTransportStateCIO", "-d", "2" })
  resyncState.tasks = { profiler, registry }
  profiler:start()
  registry:start()
end

-- (Re)starts the settle timer while inside the post-wake window.
local function scheduleResync(reason)
  local now = hs.timer.secondsSinceEpoch()
  if not resyncState.awake or now > resyncState.wakeAt + DISPLAY_RESYNC.windowSec then return end
  if resyncState.count >= DISPLAY_RESYNC.maxPerWake then return end
  if resyncState.timer then resyncState.timer:stop() end
  resyncState.timer = hs.timer.doAfter(DISPLAY_RESYNC.settleSec, function()
    resyncState.timer = nil
    if not resyncState.awake then return end
    resyncIfEligible(reason, function(count)
      if count > 0 then resyncState.count = resyncState.count + 1 end
    end, true)
  end)
end

local resyncPowerWatcher = hs.caffeinate.watcher.new(function(event)
  local w = hs.caffeinate.watcher
  if event == w.screensDidSleep or event == w.systemWillSleep then
    resyncState.awake = false
    if resyncState.timer then resyncState.timer:stop(); resyncState.timer = nil end
    -- Start drop-out tracking from the screens present when the displays slept.
    resyncState.tracked, resyncState.missing, resyncState.reconnected = currentScreenUUIDs(), {}, {}
  elseif event == w.screensDidWake or event == w.systemDidWake then
    resyncState.awake = true
    resyncState.wakeAt = hs.timer.secondsSinceEpoch()
    resyncState.count = 0
    -- Record a monitor that macOS already dropped at wake; act once it is back
    -- (or now, if it somehow came back while the displays were asleep).
    if trackScreens(resyncState, currentScreenUUIDs(), false) or next(resyncState.reconnected) then
      scheduleResync("monitor reconnected after wake")
    end
  end
end)
resyncState.tracked = currentScreenUUIDs()
resyncPowerWatcher:start()
-- Learn how the monitors reach the dock now, while they are settled.
resyncIfEligible("startup", nil, false, true)

local resyncScreenWatcher = hs.screen.watcher.new(function()
  local busy = hs.timer.secondsSinceEpoch() < resyncState.busyUntil
  if trackScreens(resyncState, currentScreenUUIDs(), busy) then
    scheduleResync("monitor reconnected after wake")
  end
end)
resyncScreenWatcher:start()
_G.HS_KEEPALIVE.watchers.resyncPowerWatcher = resyncPowerWatcher
_G.HS_KEEPALIVE.watchers.resyncScreenWatcher = resyncScreenWatcher

-- <WM_MOD>r: re-sync the qualifying monitor now (same checks and switch).
local resyncHotkey = keepHotkey(WM_MOD, "r", function()
  resyncIfEligible("hotkey", function(count, why, targets)
    if count > 0 then
      local names = {}
      for _, scr in ipairs(targets) do names[#names + 1] = tostring(scr:name()) end
      hs.alert.show("Re-syncing " .. table.concat(names, ", "), 1)
    elseif #targets > 0 then
      hs.alert.show("Display re-sync: no other refresh rate to switch to", 2)
    else
      hs.alert.show("Display re-sync skipped: " .. why, 2)
    end
  end, false)
end)

-- -- Wake values, warm-up brightness, <WM_MOD>6/7 and <WM_MOD>F6/F7 (left monitor) -
-- That CR240DM's backlight gives out above brightness 25 (of 100) while it is
-- cold: minutes after a 2.5h display sleep it went blank at 26 -- the same
-- blank-but-connected state a re-sync clears -- yet ran fine at 75 about 12
-- minutes after the wake. So when the displays wake after sleeping an hour or
-- more, the left monitor gets contrast 50 right away and brightness 25 while it
-- warms up, then brightness 50 (unless its brightness was changed meanwhile);
-- shorter sleeps leave both alone. Its red, green and blue gains go back to 50
-- at every wake: it has woken up with its brightness, and once its green and
-- blue gains, at 2 -- also saved by BetterDisplay, which re-applies its saved
-- DDC values on wake. <WM_MOD>6 / <WM_MOD>7 step its brightness and
-- <WM_MOD>F6 / <WM_MOD>F7 its contrast down / up by 25; each takes that control
-- over from the warm-up, whose other control carries on. Both use BetterDisplay's
-- direct DDC access (VCP luminance, contrast and the RGB gains: the 0-100 values
-- of the monitor's own menu) through its notification-based integration, so
-- BetterDisplay must be running. Everything lives in one table: Lua caps a chunk
-- at 200 locals, and this one is close to that.
local leftMonitor = {
  config = {
    minSleepSec = 3600,  -- warm up only after the displays slept at least this long
    warmupLevel = 25,    -- brightness held while cold (it went blank at 26)
    warmupSec = 600,     -- then the brightness goes to `brightness`
    brightness = 50,     -- after the warm-up
    contrast = 50,       -- right at the wake
    color = 50,          -- red, green and blue gains, at every wake
    waitSec = 60,        -- for the monitor to come back (and answer DDC) after a wake
    step = 25,           -- <WM_MOD>6/7 (brightness), <WM_MOD>F6/F7 (contrast)
  },
  colorVCPs = { { "red", "0x16" }, { "green", "0x18" }, { "blue", "0x1A" } },  -- gain VCP codes
  pending = {}, seq = 0,  -- BetterDisplay requests awaiting a response
  sleptAt = nil,          -- when the displays went to sleep
  targetUUID = nil,       -- the left monitor's UUID at that moment
  pendingLevel = nil,     -- brightness to set once warm (nil: none pending)
  holdContrast = false,   -- the warm-up still owns the contrast
  token = 0,              -- bumped to cancel the warm-up steps in flight
  timer = nil,            -- next warm-up step (strong reference)
  lastStep = {},          -- vcp -> { uuid, level, at } of the last hotkey step
  alert = nil,            -- on-screen level of the last step, replaced by the next
}

-- JSON request for BetterDisplay: {uuid, commands, parameters}, keys sorted.
-- Parameter values are plain tokens (UUIDs, names, numbers); `true` sends a
-- parameter without a value (null).
function leftMonitor.requestJSON(id, commands, params)
  local keys, fields = {}, {}
  for k in pairs(params) do keys[#keys + 1] = k end
  table.sort(keys)
  for i, k in ipairs(keys) do
    local v = params[k]
    fields[i] = string.format('"%s":%s', k, v == true and "null" or ('"' .. tostring(v) .. '"'))
  end
  return string.format('{"uuid":"%s","commands":["%s"],"parameters":{%s}}',
    id, table.concat(commands, '","'), table.concat(fields, ","))
end

-- Sends a request to BetterDisplay; callback(ok, payload) once it answers, or
-- callback(false, nil) after 3s (BetterDisplay not running, integration disabled).
function leftMonitor.request(commands, params, callback)
  leftMonitor.seq = leftMonitor.seq + 1
  local id = "hs-leftmonitor-" .. leftMonitor.seq
  local entry = { callback = callback }
  entry.timer = hs.timer.doAfter(3, function()
    if leftMonitor.pending[id] then
      leftMonitor.pending[id] = nil
      callback(false, nil)
    end
  end)
  leftMonitor.pending[id] = entry
  hs.distributednotifications.post("pro.betterdisplay.BetterDisplay.request",
    leftMonitor.requestJSON(id, commands, params))
end

leftMonitor.responseWatcher = hs.distributednotifications.new(function(_, object)
  local ok, resp = pcall(hs.json.decode, object or "")
  local entry = ok and type(resp) == "table" and resp.uuid and leftMonitor.pending[resp.uuid]
  if not entry then return end
  leftMonitor.pending[resp.uuid] = nil
  entry.timer:stop()
  entry.callback(resp.result == true, resp.payload)
end, "pro.betterdisplay.BetterDisplay.response")
leftMonitor.responseWatcher:start()

-- Gets (value nil) or sets a DDC control of the monitor through BetterDisplay
-- (vcp "luminance", "contrast" or a VCP code such as "0x18", 0-100); calls back
-- with the level it reports, or nil on failure.
function leftMonitor.ddc(uuid, vcp, value, callback)
  local params = { UUID = uuid, feature = "ddc", vcp = vcp, value = value and math.floor(value) or true }
  leftMonitor.request(value and { "set", "get" } or { "get" }, params, function(ok, payload)
    callback(ok and tonumber(tostring(payload or ""):match("^%s*(%d+)")) or nil)
  end)
end

-- Console line about the left monitor (a field, so tests can capture it).
function leftMonitor.log(msg) resyncLog.i(msg) end

-- The left monitor: the leftmost screen of the DISPLAY_RESYNC.monitorName model.
function leftMonitor.screen()
  local left = nil
  for _, scr in ipairs(hs.screen.allScreens()) do
    local name = modelName(scr)
    if not name:find("^Built%-in") and (DISPLAY_RESYNC.monitorName == nil or name == DISPLAY_RESYNC.monitorName)
       and (not left or scr:frame().x < left:frame().x) then
      left = scr
    end
  end
  return left
end

-- Level after a hotkey step, within 0-100.
function leftMonitor.stepAfter(level, delta)
  return math.max(0, math.min(100, math.floor(level + delta + 0.5)))
end

-- <WM_MOD>6/7 (vcp "luminance") and <WM_MOD>F6/F7 ("contrast"). The stepped
-- control is taken over from the warm-up (its pending brightness step, or the
-- contrast it holds). Presses in quick succession build on the level just set
-- (BetterDisplay may not have re-read the monitor yet).
function leftMonitor.step(vcp, delta)
  local label = "Left monitor " .. (vcp == "contrast" and "contrast" or "brightness")
  local scr = leftMonitor.screen()
  if not scr then
    hs.alert.show(label .. ": no " .. (DISPLAY_RESYNC.monitorName or "external monitor") .. " connected", 2)
    return
  end
  if vcp == "contrast" then leftMonitor.holdContrast = false else leftMonitor.pendingLevel = nil end
  local uuid = scr:getUUID()
  local function apply(level)
    if not level then
      hs.alert.show(label .. ": BetterDisplay did not respond", 2)
      return
    end
    local target = leftMonitor.stepAfter(level, delta)
    leftMonitor.lastStep[vcp] = { uuid = uuid, level = target, at = hs.timer.secondsSinceEpoch() }
    leftMonitor.ddc(uuid, vcp, target, function(now)
      if leftMonitor.alert then hs.alert.closeSpecific(leftMonitor.alert) end
      leftMonitor.alert = hs.alert.show(label .. ": " .. (now or "BetterDisplay did not respond"), {}, scr, 1)
    end)
  end
  local last = leftMonitor.lastStep[vcp]
  if last and last.uuid == uuid and hs.timer.secondsSinceEpoch() - last.at < 3 then
    apply(last.level)
  else
    leftMonitor.ddc(uuid, vcp, nil, apply)
  end
end

-- After a display sleep: wait for the left monitor (the one that was left when
-- the displays slept) to answer DDC, then put each control this still owns back
-- to its wake value -- checked again after 10s and 30s, as BetterDisplay
-- re-applies its own settings after a wake. Its red, green and blue gains are
-- owned at every wake; after a long sleep (long) also its contrast and the
-- warm-up brightness, raised to `brightness` after warmupSec unless it was
-- changed meanwhile. A warm-up that a shorter display sleep interrupted resumes
-- its brightness step (the contrast was set at the first wake).
function leftMonitor.startWarmup(asleepFor, long)
  local cfg = leftMonitor.config
  leftMonitor.token = leftMonitor.token + 1
  if long then leftMonitor.pendingLevel = cfg.brightness end
  leftMonitor.holdContrast = long
  local token, uuid = leftMonitor.token, leftMonitor.targetUUID
  local holdColors = true
  local deadline = hs.timer.secondsSinceEpoch() + cfg.waitSec
  -- { label, vcp, value it must have now (nil: not, or no longer, owned) }
  local controls = {
    { "brightness", "luminance", function() return leftMonitor.pendingLevel and cfg.warmupLevel end },
    { "contrast", "contrast", function() return leftMonitor.holdContrast and cfg.contrast end },
  }
  for _, c in ipairs(leftMonitor.colorVCPs) do
    controls[#controls + 1] = { c[1], c[2], function() return holdColors and cfg.color end }
  end
  local function after(sec, fn)
    if leftMonitor.timer then leftMonitor.timer:stop() end
    leftMonitor.timer = hs.timer.doAfter(sec, function()
      leftMonitor.timer = nil
      if token == leftMonitor.token then fn() end
    end)
  end
  local function read(vcp, fn)  -- fn(level or nil); dropped once cancelled
    leftMonitor.ddc(uuid, vcp, nil, function(level)
      if token == leftMonitor.token then fn(level) end
    end)
  end
  local function write(vcp, value, fn)  -- fn(level it reports after); likewise
    leftMonitor.ddc(uuid, vcp, value, function(level)
      if token == leftMonitor.token then fn(level) end
    end)
  end
  local function name()
    local scr = findScreenByUUID(uuid)
    return scr and scr:name() or "left monitor"
  end
  -- Puts each control still owned back to its value, one at a time, then calls
  -- done(changes): "label old -> new" for each one that was off.
  local function enforce(done)
    local changes, i = {}, 0
    local function nextControl()
      i = i + 1
      local c = controls[i]
      if not c then return done(changes) end
      if not c[3]() then return nextControl() end
      read(c[2], function(level)
        local value = c[3]()  -- a hotkey may have taken it over meanwhile
        if not (value and level and level ~= value) then return nextControl() end
        write(c[2], value, function(now)
          changes[#changes + 1] = string.format("%s %d -> %s", c[1], level, tostring(now))
          nextControl()
        end)
      end)
    end
    nextControl()
  end
  -- done callback of a later check: logs what it put back, then runs fn.
  local function report(when, fn)
    return function(changes)
      if #changes > 0 then
        leftMonitor.log(string.format("wake: %s %s: %s", name(), when, table.concat(changes, ", ")))
      end
      fn()
    end
  end
  local finishTries = 0
  local function finish()
    leftMonitor.holdContrast = false
    if not leftMonitor.pendingLevel then return end
    read("luminance", function(level)
      if not level and finishTries < 6 then  -- no DDC answer: try again shortly
        finishTries = finishTries + 1
        return after(5, finish)
      end
      local target = leftMonitor.pendingLevel
      leftMonitor.pendingLevel = nil
      if not target then return end
      if level ~= cfg.warmupLevel then
        leftMonitor.log(string.format("warm-up: %s brightness is %s, leaving it (changed meanwhile)",
          name(), tostring(level)))
        return
      end
      leftMonitor.ddc(uuid, "luminance", target, function(now)
        leftMonitor.log(string.format("warm-up: %s brightness %d -> %s", name(), cfg.warmupLevel, tostring(now)))
      end)
    end)
  end
  local start
  local function retry(why)
    if hs.timer.secondsSinceEpoch() < deadline then return after(1, start) end
    if leftMonitor.pendingLevel or leftMonitor.holdContrast then  -- quiet if only the gains were due
      leftMonitor.log("warm-up: skipped (" .. why .. ")")
    end
  end
  start = function()
    if not findScreenByUUID(uuid) then return retry("the left monitor did not come back") end
    read("luminance", function(level)
      if not level then return retry("no DDC answer from " .. name()) end
      enforce(function(changes)
        if leftMonitor.pendingLevel then
          changes[#changes + 1] = string.format("then brightness %d after %ds",
            leftMonitor.pendingLevel, cfg.warmupSec)
        end
        if #changes > 0 then
          leftMonitor.log(string.format("wake: %s %s (displays slept %dm)",
            name(), table.concat(changes, ", "), math.floor(asleepFor / 60)))
        end
        after(10, function()
          enforce(report("+10s", function()
            after(20, function()
              enforce(report("+30s", function()
                holdColors = false
                if leftMonitor.pendingLevel then
                  after(math.max(1, cfg.warmupSec - 30), finish)
                else
                  leftMonitor.holdContrast = false
                end
              end))
            end)
          end))
        end)
      end)
    end)
  end
  start()
end

-- Display sleep/wake (now: injectable clock for tests).
function leftMonitor.onPowerEvent(event, now)
  local w = hs.caffeinate.watcher
  now = now or hs.timer.secondsSinceEpoch()
  if event == w.screensDidSleep then
    -- Pause a warm-up in progress (the next wake resumes its brightness step) and
    -- note which monitor is the left one while every screen is still connected
    -- (no CR240DM: nothing to do at the wake).
    leftMonitor.token = leftMonitor.token + 1
    if leftMonitor.timer then leftMonitor.timer:stop(); leftMonitor.timer = nil end
    if not leftMonitor.sleptAt then
      leftMonitor.sleptAt = now
      local scr = leftMonitor.screen()
      leftMonitor.targetUUID = scr and scr:getUUID() or nil
    end
  elseif event == w.screensDidWake and leftMonitor.sleptAt then
    local asleepFor = now - leftMonitor.sleptAt
    leftMonitor.sleptAt = nil
    if leftMonitor.targetUUID then
      leftMonitor.startWarmup(asleepFor, asleepFor >= leftMonitor.config.minSleepSec)
    end
  end
end

leftMonitor.powerWatcher = hs.caffeinate.watcher.new(function(event) leftMonitor.onPowerEvent(event) end)
leftMonitor.powerWatcher:start()
_G.HS_KEEPALIVE.watchers.leftMonitorPowerWatcher = leftMonitor.powerWatcher
_G.HS_KEEPALIVE.watchers.betterDisplayResponseWatcher = leftMonitor.responseWatcher

leftMonitor.hotkeys = {
  keepHotkey(WM_MOD, "6", function() leftMonitor.step("luminance", -leftMonitor.config.step) end),
  keepHotkey(WM_MOD, "7", function() leftMonitor.step("luminance", leftMonitor.config.step) end),
  keepHotkey(WM_MOD, "f6", function() leftMonitor.step("contrast", -leftMonitor.config.step) end),
  keepHotkey(WM_MOD, "f7", function() leftMonitor.step("contrast", leftMonitor.config.step) end),
}

-- Global for live IPC testing and manual use: `displayResync.resyncNow()`
-- re-syncs the qualifying monitor right away (no drop-out needed) and logs it.
displayResync = {
  config = DISPLAY_RESYNC,
  state = resyncState,
  alternateMode = alternateMode,
  displaySerials = displaySerials,
  dockDisplayPortSinks = dockDisplayPortSinks,
  eligibleScreens = eligibleScreens,
  trackScreens = trackScreens,
  currentScreenUUIDs = currentScreenUUIDs,
  hotkey = resyncHotkey,
  leftMonitor = leftMonitor,
  resyncNow = function() resyncIfEligible("manual", nil, false) end,
}
