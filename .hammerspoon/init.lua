-- =============================================================================
-- ~/.hammerspoon/init.lua: Dual-OS Window Manager & X11/XFCE4 Parity Engine
-- =============================================================================
-- Replicates Marcel's MTV workstation and WFH laptop X11/XFCE4 window management
-- workflows natively on macOS without hijacking standard macOS Cmd+Z/C/V/X/K/N:
--   1. Stateful 6-tier fractional window cycling (x-window-move port):
--      {1.0, 0.75, 0.666, 0.5, 0.334, 0.25} on WM_MOD (Ctrl+Cmd) + Arrows/K.
--   2. Cross-monitor window movement & dual-screen spanning (both-screens) on
--      both WFH (<Ctrl><Cmd>Page_Up) and MTV (<Alt><Cmd>Left/Right/Page_Up).
--   3. 6-Space workspace navigation (<Ctrl><Alt>1..6) and window migration that
--      follows the window (<Ctrl><Shift><Alt>Left/Right and <Ctrl><Shift><Alt>1..6).
--   4. Sticky Window toggle (<Alt>F6) + automatic Sticky Kitty watcher via
--      hs.spaces.watcher (bypassing macOS SIP restrictions on third-party apps).
--   5. Borderless Alt+Left-Drag window move & Alt+Right-Drag window resize
--      (replicating xfwm4 easy_click=Alt).
--   6. <Alt>Tab MRU fuzzy window switcher (hs.chooser) with live 10px 85%-opaque
--      blue highlight border (hs.canvas, replicating draw_border_around_window.py).
--   7. <WM_MOD>z Zen Mode toggle & <WM_MOD>n terminal-scoped Notes launcher.
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
-- 3. 6-Space Workspace Switching (<Ctrl><Alt>1..6) & Window-to-Space Moves that
--    follow the window (<Ctrl><Shift><Alt>Left/Right & <Ctrl><Shift><Alt>1..6)
-- -----------------------------------------------------------------------------
-- macOS 14.5+ (15 Sequoia, 26 Tahoe) NOPs CGSMoveWindowsToManagedSpace, so
-- hs.spaces.moveWindowToSpace() returns true while the window stays put. Windows
-- are therefore moved the way a human moves them (verified live on macOS 26.7):
--   A. Titled windows: hold the window by a draggable titlebar point (synthetic
--      leftMouseDown + leftMouseDragged) and post the user's *own* macOS
--      "Move left/right a space" (symbolic hotkeys 79/81) or "Switch to Desktop N"
--      (118..133) shortcut, read live from com.apple.symbolichotkeys (e.g.
--      Ctrl+Alt+Arrow on the gMac rather than the stock Ctrl+Arrow; arrow-key
--      shortcuts must carry the fn flag). WindowServer carries the held window to
--      the new Space. hs.spaces.gotoSpace() switches via a Mission Control AXPress
--      and does NOT carry a held window, so it cannot be used for this.
--   B. Borderless windows (kitty hide_window_decorations yes), titlebars with no
--      draggable spot, or no enabled Spaces shortcut: open Mission Control, wait
--      for the thumbnails to settle, press the window's thumbnail and lift it (the
--      Spaces bar expands only once a drag is in progress), drop it onto the
--      *expanded* target Space thumbnail, then AXPress that Space to follow it.
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
local symbolicHotkeysCache, symbolicHotkeysReadAt = nil, 0

local function readSymbolicHotkeys()
  local now = hs.timer.secondsSinceEpoch()
  -- Cached for 5 minutes; also invalidated whenever a posted shortcut has no effect
  if symbolicHotkeysCache and (now - symbolicHotkeysReadAt) < 300 then
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
    data = hs.plist.read(os.getenv("HOME") .. "/Library/Preferences/com.apple.symbolichotkeys.plist")
  end
  symbolicHotkeysCache = (type(data) == "table" and data.AppleSymbolicHotKeys) or {}
  symbolicHotkeysReadAt = now
  return symbolicHotkeysCache
end

-- Returns { keycode = n, flags = n } for an enabled symbolic hotkey id, or nil.
local function getSymbolicHotkey(id)
  local entry = readSymbolicHotkeys()[tostring(id)]
  if entry == nil then
    return SYMBOLIC_HOTKEY_DEFAULTS[id]
  end
  if entry.enabled == false or entry.enabled == 0 then return nil end
  local params = type(entry.value) == "table" and entry.value.parameters or nil
  local keycode = type(params) == "table" and tonumber(params[2]) or nil
  if not keycode or keycode == 65535 then return nil end
  return { keycode = math.floor(keycode), flags = math.floor(tonumber(params[3]) or 0) }
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
local finishMove, startPendingMove  -- forward declarations

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
  down:post()
  local up = hs.eventtap.event.newKeyEvent(hotkey.keycode, false)
  up:rawFlags(hotkey.flags)
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
  if op.mcOpen and not ok then pcall(hs.spaces.closeMissionControl) end
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

-- Picks win's thumbnail by title, then aspect ratio (thumbnails are scaled-down
-- copies), then Mission Control's front-to-back order (win was focused first).
-- Refuses to guess when neither title nor shape identifies the window.
local function findWindowThumbnail(thumbs, winTitle, winFrame)
  local aspect = winFrame.w / math.max(1, winFrame.h)
  local scored = {}
  for idx, th in ipairs(thumbs) do
    local tf = th.AXFrame
    if tf and tf.w > 0 and tf.h > 0 then
      local title = th.AXTitle or ""
      local titleScore = 0
      if winTitle ~= "" and title == winTitle then
        titleScore = 2
      elseif winTitle ~= "" and mcTitleMatches(title, winTitle) then
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

local function moveWindowViaMissionControl(op)
  local win, scr = op.win, op.scr
  op.method = (op.method and op.method ~= "mission-control") and (op.method .. "+mission-control") or "mission-control"
  local full = scr:fullFrame()
  local winTitle = win:title() or ""
  local winFrame = win:frame()
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
    local _, tf = findWindowThumbnail(getMissionControlWindows(display), winTitle, winFrame)
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
                  -- 4. Verify, then follow the window by pressing the target Space
                  opPoll(op, 0.05, 1.5, function()
                    return indexOf(hs.spaces.windowSpaces(win) or {}, op.targetSpace)
                  end, function(dropped)
                    trace(op, dropped and "drop moved the window" or "drop did not move the window")
                    if not dropped then
                      return finishMove(op, false, "Mission Control drop did not move the window")
                    end
                    local sp = getMissionControlSpaces(getMissionControlDisplay(scr))[spaceListIdx]
                    local okPress, pressed = pcall(function() return sp and sp:performAction("AXPress") end)
                    trace(op, "AXPress target Space: " .. ((okPress and pressed) and "ok" or "failed"))
                    if not (okPress and pressed) then
                      pcall(hs.spaces.closeMissionControl)
                      pcall(hs.spaces.gotoSpace, op.targetSpace)
                    end
                    op.mcOpen = false
                    opPoll(op, 0.05, 2.5, function()
                      return hs.spaces.focusedSpace() == op.targetSpace
                    end, function(arrived)
                      finishMove(op, true, arrived and "dragged in Mission Control"
                        or "dragged in Mission Control (Space switch still pending)")
                    end)
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
local function planSpaceSwitchKeys(scr, fromSpace, targetSpace, targetIdx)
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
    showWorkspaceBanner(scr, idx, #spaces)
    if targetSpace == hs.spaces.activeSpaceOnScreen(scr) then return end
    -- Let the banner paint before gotoSpace() busy-waits on Mission Control
    if workspaceSwitchTimer then workspaceSwitchTimer:stop() end
    workspaceSwitchTimer = hs.timer.doAfter(0.01, function()
      workspaceSwitchTimer = nil
      hs.spaces.gotoSpace(targetSpace)
    end)
  end
end

local function moveFocusedWindowToWorkspaceIndex(idx)
  requestSpaceMove("index", idx)
end

local function moveFocusedWindowToAdjacentSpace(delta)
  requestSpaceMove("delta", delta)
end

for i = 1, 6 do
  bindWorkspaceHotkey({"ctrl", "alt"}, tostring(i), function()
    switchToWorkspaceIndex(i)
  end)
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
--   - Pressing <Alt>Tab only queries hs.window.orderedWindows() on the active
--     Space (< 2ms) and reads other workspaces from in-memory cache (zero
--     hs.window.get / AXUIElement IPC or WindowServer scans on startup).
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

-- Background-only sync of windows across all Spaces (never run on <Alt>Tab critical path)
local function syncOtherSpacesWindowsAsync()
  local spaceIndexById, focusedSpaceId, currentSpaceIdx = refreshSpaceIndexCache()

  local activeIds = {}
  for _, w in ipairs(hs.window.orderedWindows()) do
    if w:isStandard() and w:isVisible() then
      local wid = recordActiveWindowFast(w, focusedSpaceId, currentSpaceIdx)
      if wid then activeIds[wid] = true end
    end
  end

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
    if activeIds[wid] then
      meta.spaceId = focusedSpaceId
      meta.spaceIdx = currentSpaceIdx
      meta.isSticky = (stickyWindows[wid] == true) or ((spaceCountByWinId[wid] or 0) > 1)
    elseif spId then
      meta.spaceId = spId
      meta.spaceIdx = spaceIndexById[spId] or meta.spaceIdx or 1
      meta.isSticky = (stickyWindows[wid] == true) or ((spaceCountByWinId[wid] or 0) > 1)
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
  local wid = recordActiveWindowFast(win, cachedCurrentSpaceId, cachedCurrentSpaceIdx)
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

-- Fast O(1) choice builder called on <Alt>Tab: only queries hs.window.orderedWindows()
-- on the active Space (< 2ms) and merges cached windows from other workspaces.
local function buildAllWorkspacesChooserChoices()
  local currentSpaceIdx = cachedCurrentSpaceIdx or 1
  local focusedSpaceId = cachedCurrentSpaceId

  local activeOrdered = hs.window.orderedWindows()
  local activeSet = {}
  for _, w in ipairs(activeOrdered) do
    if w:isStandard() and w:isVisible() then
      local wid = recordActiveWindowFast(w, focusedSpaceId, currentSpaceIdx)
      if wid then activeSet[wid] = true end
    end
  end

  local validEntriesById = {}
  local candidateIds = {}
  for wid, meta in pairs(knownWindowsById) do
    local onActive = (activeSet[wid] == true)
    if onActive or (meta.spaceIdx and meta.spaceIdx ~= currentSpaceIdx) then
      local isSticky = (stickyWindows[wid] == true) or (meta.isSticky == true)
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
    if entry and id ~= focusedWinId and entry.displaySpaceIdx == currentSpaceIdx then
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
  local function pushId(id)
    if id and validEntriesById[id] and not seen[id] then
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
    local subText = string.format(
      "Workspace %d%s | %s | PID: %d | Window ID: %d",
      meta.displaySpaceIdx, stickyTag, appName, meta.pid or 0, wid
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
      onCurrentSpace = meta.onCurrentSpace,
      frame = meta.frame,
      bundleID = meta.bundleID,
      appName = appName,
      textLower = textLower,
      searchText = searchText,
    })
  end

  return choices, currentSpaceIdx
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

local function focusSelectedWindowChoice(choice)
  if not choice or not choice.winId then return end
  local winId = choice.winId
  local targetSpaceId = choice.spaceId
  local targetSpaceIdx = choice.spaceIdx

  local function raiseAndFocusTarget()
    local meta = knownWindowsById[winId]
    local w = (meta and meta.win) or hs.window.get(winId)
    if w then
      local okFocus = pcall(function()
        if w:isMinimized() then w:unminimize() end
        local app = w:application()
        if app then app:activate() end
        w:raise()
        w:focus()
      end)
      if okFocus then return true end
    end
    local app = (choice.bundleID and hs.application.get(choice.bundleID)) or
                (choice.appName and hs.application.get(choice.appName))
    if app then
      app:activate()
      for _, aw in ipairs(app:allWindows()) do
        if aw:id() == winId then
          aw:raise()
          aw:focus()
          return true
        end
      end
    end
    return false
  end

  -- Same workspace (or sticky across all workspaces): raise and focus immediately
  if choice.onCurrentSpace or choice.isSticky or not targetSpaceIdx or targetSpaceIdx == cachedCurrentSpaceIdx then
    raiseAndFocusTarget()
    return
  end

  -- Different workspace: switch to the target workspace first, then raise & focus window
  if targetSpaceIdx >= 1 and targetSpaceIdx <= 6 then
    switchToWorkspaceIndex(targetSpaceIdx)
  elseif targetSpaceId then
    pcall(hs.spaces.gotoSpace, targetSpaceId)
  end

  local attempts = 0
  local focusTimer = nil
  focusTimer = hs.timer.doEvery(0.05, function()
    attempts = attempts + 1
    local nowSpace = hs.spaces.focusedSpace()
    if (targetSpaceId and nowSpace == targetSpaceId) or attempts >= 10 then
      if focusTimer then
        focusTimer:stop()
        focusTimer = nil
      end
      raiseAndFocusTarget()
      hs.timer.doAfter(0.08, raiseAndFocusTarget)
    end
  end)
end

windowChooser = hs.chooser.new(function(choice)
  clearWindowBorder()
  focusSelectedWindowChoice(choice)
end)
windowChooser:bgDark(true)
windowChooser:searchSubText(true)

windowChooser:queryChangedCallback(function(newQuery)
  local filtered = filterChooserChoices(allChooserChoices, newQuery)
  windowChooser:choices(filtered)
  lastSelectedRow = nil
  scheduleBorderUpdate()
end)

local function openWindowSwitcherChooser()
  local choices, currentSpaceIdx = buildAllWorkspacesChooserChoices()
  allChooserChoices = choices
  initialChooserQuery = string.format("^%d ", currentSpaceIdx)

  windowChooser:choices(filterChooserChoices(allChooserChoices, initialChooserQuery))
  windowChooser:query(initialChooserQuery)
  windowChooser:show()
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
