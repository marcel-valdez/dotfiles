-- =============================================================================
-- ~/.hammerspoon/init.lua: Dual-OS Window Manager & X11/XFCE4 Parity Engine
-- =============================================================================
-- Replicates Marcel's MTV workstation and WFH laptop X11/XFCE4 window management
-- workflows natively on macOS without hijacking standard macOS Cmd+Z/C/V/X/K/N:
--   1. Stateful 6-tier fractional window cycling (x-window-move port):
--      {1.0, 0.75, 0.666, 0.5, 0.334, 0.25} on WM_MOD (Ctrl+Cmd) + Arrows/K.
--   2. Cross-monitor window movement & dual-screen spanning (both-screens) on
--      both WFH (<Ctrl><Cmd>Page_Up) and MTV (<Alt><Cmd>Left/Right/Page_Up).
--   3. 6-Space workspace navigation (<Ctrl><Alt>1..6) and window migration
--      (<Ctrl><Shift><Alt>Left/Right).
--   4. Sticky Window toggle (<Alt>F6) + automatic Sticky Kitty watcher via
--      hs.spaces.watcher (bypassing macOS SIP restrictions on third-party apps).
--   5. Borderless Alt+Left-Drag window move & Alt+Right-Drag window resize
--      (replicating xfwm4 easy_click=Alt).
--   6. <Alt>Tab MRU fuzzy window switcher (hs.chooser) with live 10px 85%-opaque
--      blue highlight border (hs.canvas, replicating draw_border_around_window.py).
--   7. <WM_MOD>z Zen Mode toggle & <WM_MOD>n terminal-scoped Notes launcher.
-- =============================================================================

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
hs.hotkey.bind(WM_MOD,       "Left",  function() cycleWindowSnap("left",   false) end)
hs.hotkey.bind(WM_MOD_SHIFT, "Left",  function() cycleWindowSnap("left",   true)  end)
hs.hotkey.bind(WM_MOD,       "Right", function() cycleWindowSnap("right",  false) end)
hs.hotkey.bind(WM_MOD_SHIFT, "Right", function() cycleWindowSnap("right",  true)  end)
hs.hotkey.bind(WM_MOD,       "Up",    function() cycleWindowSnap("up",     false) end)
hs.hotkey.bind(WM_MOD_SHIFT, "Up",    function() cycleWindowSnap("up",     true)  end)
hs.hotkey.bind(WM_MOD,       "Down",  function() cycleWindowSnap("down",   false) end)
hs.hotkey.bind(WM_MOD_SHIFT, "Down",  function() cycleWindowSnap("down",   true)  end)
hs.hotkey.bind(WM_MOD,       "k",     function() cycleWindowSnap("center", false) end)
hs.hotkey.bind(WM_MOD_SHIFT, "k",     function() cycleWindowSnap("center", true)  end)

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
hs.hotkey.bind(ALT_SUPER,    "Left",     function() moveWindowToScreenInDirection("west") end)
hs.hotkey.bind(ALT_SUPER,    "Right",    function() moveWindowToScreenInDirection("east") end)
hs.hotkey.bind(ALT_SUPER,    "pageup",   toggleSpanBothScreens)
hs.hotkey.bind(WM_MOD,       "pageup",   toggleSpanBothScreens)
hs.hotkey.bind(WM_MOD_SHIFT, "pageup",   function()
  local win = hs.window.focusedWindow()
  if win then win:maximize() end
end)
hs.hotkey.bind(WM_MOD,       "pagedown", function()
  local win = hs.window.focusedWindow()
  if win then win:minimize() end
end)

-- -----------------------------------------------------------------------------
-- 3. 6-Space Workspace Switching (<Ctrl><Alt>1..6 & <Ctrl><Shift><Alt>Left/Right)
-- -----------------------------------------------------------------------------
local function getOrderedUserSpacesForCurrentScreen()
  local scr = hs.screen.mainScreen()
  if not scr then return {} end
  local allSpaces = hs.spaces.allSpaces()
  local uuid = scr:getUUID()
  local rawSpaces = allSpaces and allSpaces[uuid] or {}
  local userSpaces = {}
  for _, spId in ipairs(rawSpaces) do
    if hs.spaces.spaceType(spId) == "user" then
      table.insert(userSpaces, spId)
    end
  end
  return userSpaces
end

local function switchToWorkspaceIndex(idx)
  local spaces = getOrderedUserSpacesForCurrentScreen()
  local targetSpace = spaces[idx]
  if targetSpace then
    hs.spaces.gotoSpace(targetSpace)
  end
end

local function moveFocusedWindowToAdjacentSpace(delta)
  local win = hs.window.focusedWindow()
  if not win then return end
  local spaces = getOrderedUserSpacesForCurrentScreen()
  local currentSpace = hs.spaces.focusedSpace()
  local currentIdx = nil
  for i, spId in ipairs(spaces) do
    if spId == currentSpace then
      currentIdx = i
      break
    end
  end
  if not currentIdx then return end
  local targetIdx = currentIdx + delta
  if targetIdx >= 1 and targetIdx <= #spaces then
    local targetSpace = spaces[targetIdx]
    hs.spaces.moveWindowToSpace(win:id(), targetSpace)
    hs.spaces.gotoSpace(targetSpace)
  end
end

for i = 1, 6 do
  hs.hotkey.bind({"ctrl", "alt"}, tostring(i), function()
    switchToWorkspaceIndex(i)
  end)
end

hs.hotkey.bind({"ctrl", "shift", "alt"}, "Left",  function() moveFocusedWindowToAdjacentSpace(-1) end)
hs.hotkey.bind({"ctrl", "shift", "alt"}, "Right", function() moveFocusedWindowToAdjacentSpace(1)  end)

-- -----------------------------------------------------------------------------
-- 4. Sticky Window Toggle (<Alt>F6) & Auto-Sticky Kitty Watcher
-- -----------------------------------------------------------------------------
local stickyWindows = {}
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

local spaceWatcher = hs.spaces.watcher.new(function(_)
  hs.timer.doAfter(0.05, syncStickyWindowsToFocusedSpace)
end)
spaceWatcher:start()

hs.hotkey.bind({"alt"}, "f6", function()
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

local altDragTap = hs.eventtap.new({
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
-- 6. <Alt>Tab Fuzzy Window Switcher with Live 10px 85%-Opaque Blue Border
-- -----------------------------------------------------------------------------
local mruWindowIds = {}
local highlightCanvas = nil
local windowChooser = nil
local chooserPollTimer = nil
local lastSelectedRow = nil

local wf = hs.window.filter.new()
wf:subscribe(hs.window.filter.windowFocused, function(win)
  if not win or not win:id() then return end
  local id = win:id()
  for i = #mruWindowIds, 1, -1 do
    if mruWindowIds[i] == id then
      table.remove(mruWindowIds, i)
    end
  end
  table.insert(mruWindowIds, 1, id)
end)

local function clearWindowBorder()
  if chooserPollTimer then
    chooserPollTimer:stop()
    chooserPollTimer = nil
  end
  if highlightCanvas then
    highlightCanvas:delete()
    highlightCanvas = nil
  end
  lastSelectedRow = nil
end

local function drawBlueBorderAroundWindow(win)
  if highlightCanvas then
    highlightCanvas:delete()
    highlightCanvas = nil
  end
  if not win then return end
  local f = win:frame()
  if not f or f.w <= 0 or f.h <= 0 then return end

  -- Replicates draw_border_around_window.py: 10px stroke, RGBA(0, 0, 1.0, 0.85)
  local pad = 10
  highlightCanvas = hs.canvas.new({
    x = f.x - pad,
    y = f.y - pad,
    w = f.w + (pad * 2),
    h = f.h + (pad * 2),
  })
  highlightCanvas:appendElements({
    type = "rectangle",
    action = "stroke",
    strokeWidth = 10,
    strokeColor = {red = 0.0, green = 0.0, blue = 1.0, alpha = 0.85},
    roundedRectRadii = {xRadius = 4, yRadius = 4},
    frame = {x = 5, y = 5, w = f.w + 10, h = f.h + 10},
  })
  highlightCanvas:level(hs.canvas.windowLevels.overlay)
  highlightCanvas:show()
end

local function buildChooserChoices()
  local ordered = hs.window.orderedWindows()
  local byId = {}
  local orderedList = {}
  for _, w in ipairs(ordered) do
    if w:isStandard() and w:isVisible() and w:id() then
      byId[w:id()] = w
      table.insert(orderedList, w)
    end
  end

  -- Place MRU windows first, with PREV_WINDOW_ID at index 1 so <Alt>Tab, Enter
  -- immediately toggles between the two most recently focused windows.
  local seen = {}
  local sortedWins = {}
  if mruWindowIds[2] and byId[mruWindowIds[2]] then
    table.insert(sortedWins, byId[mruWindowIds[2]])
    seen[mruWindowIds[2]] = true
  end
  for _, id in ipairs(mruWindowIds) do
    if byId[id] and not seen[id] then
      table.insert(sortedWins, byId[id])
      seen[id] = true
    end
  end
  for _, w in ipairs(orderedList) do
    if not seen[w:id()] then
      table.insert(sortedWins, w)
      seen[w:id()] = true
    end
  end

  local choices = {}
  for _, w in ipairs(sortedWins) do
    local app = w:application()
    local appName = app and app:name() or "Unknown"
    local pid = app and app:pid() or 0
    local title = w:title() or ""
    local icon = app and app:bundleID() and hs.image.imageFromAppBundle(app:bundleID()) or nil
    table.insert(choices, {
      text = string.format("[%s] %s", appName, title),
      subText = string.format("PID: %d | Window ID: %d", pid, w:id()),
      image = icon,
      winId = w:id(),
    })
  end
  return choices
end

windowChooser = hs.chooser.new(function(choice)
  clearWindowBorder()
  if not choice or not choice.winId then return end
  local targetWin = hs.window.get(choice.winId)
  if targetWin then
    targetWin:focus()
  end
end)
windowChooser:bgDark(true)
windowChooser:searchSubText(true)

hs.hotkey.bind({"alt"}, "tab", function()
  local choices = buildChooserChoices()
  windowChooser:choices(choices)
  windowChooser:show()

  lastSelectedRow = nil
  chooserPollTimer = hs.timer.doEvery(0.04, function()
    if not windowChooser:isVisible() then
      clearWindowBorder()
      return
    end
    local row = windowChooser:selectedRow()
    if row ~= lastSelectedRow then
      lastSelectedRow = row
      local item = windowChooser:selectedRowContents(row)
      if item and item.winId then
        drawBlueBorderAroundWindow(hs.window.get(item.winId))
      else
        if highlightCanvas then highlightCanvas:delete(); highlightCanvas = nil end
      end
    end
  end)
end)

-- -----------------------------------------------------------------------------
-- 7. Zen Mode (<WM_MOD>z), Terminal-Scoped Notes (<WM_MOD>n), & App Shortcuts
-- -----------------------------------------------------------------------------
hs.hotkey.bind(WM_MOD, "z", function()
  hs.task.new("/opt/homebrew/bin/bash", nil, {os.getenv("HOME") .. "/scripts/zen-mode.sh"}):start()
end)

-- Terminal-scoped Notes launcher (replicates `if-terminal-has-focus ...`)
hs.hotkey.bind(WM_MOD, "n", function()
  local frontApp = hs.application.frontmostApplication()
  local name = frontApp and string.lower(frontApp:name() or "") or ""
  if name == "kitty" or name == "wezterm" or name == "iterm2" or name == "terminal" then
    hs.task.new("/opt/homebrew/bin/bash", nil, {"-lc", os.getenv("HOME") .. "/.googlerc.d/bin/kitty-ssh-launch tab"}):start()
  end
end)

-- Launchers matching xfce4-keyboard-shortcuts.xml
hs.hotkey.bind({"ctrl", "alt"}, "t", function()
  hs.application.launchOrFocus("kitty")
end)

hs.hotkey.bind({"ctrl", "alt"}, "l", function()
  hs.caffeinate.lockScreen()
end)
