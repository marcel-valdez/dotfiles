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
local altDragTap = nil
local isMovingWindowToSpace = false
local nextMouseEventNumber = 32000

local function allocMouseEventNumber()
  nextMouseEventNumber = nextMouseEventNumber + 1
  if nextMouseEventNumber >= 0x7fffffff then
    nextMouseEventNumber = 32001
  end
  return nextMouseEventNumber
end

local function postRawMouseMove(pt)
  hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.mouseMoved, pt):rawFlags(0):post()
end

local function postRawMouseButton(evType, pt, evNum, dx, dy)
  local ev = hs.eventtap.event.newMouseEvent(evType, pt)
    :rawFlags(0)
    :setProperty(hs.eventtap.event.properties.mouseEventNumber, evNum)
    :setProperty(hs.eventtap.event.properties.mouseEventClickState, 1)
    :setProperty(hs.eventtap.event.properties.mouseEventDeltaX, dx or 0)
    :setProperty(hs.eventtap.event.properties.mouseEventDeltaY, dy or 0)
  ev:post()
end

local function getOrderedUserSpacesForCurrentScreen(scr)
  scr = scr or hs.screen.mainScreen()
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

local function ensureUserSpacesCount(scr, minCount)
  scr = scr or hs.screen.mainScreen()
  if not scr then return {} end
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  while #spaces < minCount do
    local ok, added = pcall(hs.spaces.addSpaceToScreen, scr, false)
    if not ok or not added then break end
    local updated = getOrderedUserSpacesForCurrentScreen(scr)
    if #updated <= #spaces then break end
    spaces = updated
  end
  return spaces
end

-- Mission Control Accessibility tree helpers for macOS 15 Sequoia & macOS 26 Tahoe
local function getMissionControlGroup()
  local wm = hs.application.applicationsForBundleID("com.apple.WindowManager")[1]
  if wm then
    local wmEl = hs.axuielement.applicationElement(wm)
    if wmEl then
      for _, el in ipairs(wmEl) do
        if el.AXIdentifier == "mc.display" then
          return wmEl, true
        end
      end
    end
  end
  local dock = hs.application.applicationsForBundleID("com.apple.dock")[1]
  if dock then
    local dockEl = hs.axuielement.applicationElement(dock)
    if dockEl then
      for _, el in ipairs(dockEl) do
        if el.AXIdentifier == "mc" then
          return el, false
        end
      end
    end
  end
  return nil, false
end

local function getMissionControlDisplayGroups()
  local mcGroup, isWM = getMissionControlGroup()
  if not mcGroup then return nil, false end
  local rawGroups = {}
  for _, el in ipairs(mcGroup) do
    if el.AXIdentifier == "mc.display" then
      table.insert(rawGroups, el)
    end
  end
  local ordered = {}
  for _, scr in ipairs(hs.screen.allScreens()) do
    local f = scr:fullFrame()
    for i, grp in ipairs(rawGroups) do
      local gf = grp.AXFrame
      if gf and gf.x == f.x and gf.y == f.y then
        table.insert(ordered, table.remove(rawGroups, i))
        break
      end
    end
  end
  for _, grp in ipairs(rawGroups) do
    table.insert(ordered, grp)
  end
  return ordered, isWM
end

local function getMissionControlWindows()
  local groups, isWM = getMissionControlDisplayGroups()
  if not groups then return nil, false end
  local wins = {}
  for _, grp in ipairs(groups) do
    for _, el in ipairs(grp) do
      if el.AXIdentifier == "mc.windows" then
        for _, w in ipairs(el) do
          table.insert(wins, w)
        end
      elseif el.AXIdentifier and el.AXIdentifier:find("%.space%.%d+$") then
        table.insert(wins, el)
      end
    end
  end
  return wins, isWM
end

local function getMissionControlSpaces()
  local groups, isWM = getMissionControlDisplayGroups()
  if not groups then return nil, false end
  local spaces = {}
  for _, grp in ipairs(groups) do
    for _, el in ipairs(grp) do
      if el.AXIdentifier == "mc.spaces" then
        for _, sub in ipairs(el) do
          if sub.AXIdentifier == "mc.spaces.list" then
            for _, sp in ipairs(sub) do
              table.insert(spaces, sp)
            end
          end
        end
      end
    end
  end
  return spaces, isWM
end

local function getGlobalSpaceIndex(targetSpaceId)
  local layout = hs.spaces.allSpaces()
  if not layout then return nil end
  local idx = 0
  for _, scr in ipairs(hs.screen.allScreens()) do
    local uuid = scr:getUUID()
    for i, spId in ipairs(layout[uuid] or {}) do
      if spId == targetSpaceId then
        return idx + i
      end
    end
    idx = idx + #(layout[uuid] or {})
  end
  return nil
end

local function mcTitleMatches(candidate, title)
  if not candidate or not title then return false end
  if candidate == title then return true end
  local prefix, suffix = candidate:match("^(.-)…(.*)$")
  if not prefix or #prefix < 6 then return false end
  return title:sub(1, #prefix) == prefix and
         (suffix == "" or title:sub(#title - #suffix + 1) == suffix)
end

-- Universal Mission Control thumbnail drag-to-space (works on borderless Kitty
-- windows and all macOS 14.5+ / 15 Sequoia / 26 Tahoe windows).
local function moveWindowViaMissionControl(win, targetSpaceId, switchToSpace)
  if isMovingWindowToSpace or not win then return end
  isMovingWindowToSpace = true
  local savedCursor = hs.mouse.absolutePosition()
  local wasDragTapEnabled = altDragTap and altDragTap:isEnabled()
  if wasDragTapEnabled then altDragTap:stop() end

  local function cleanupAndFinish()
    if wasDragTapEnabled and altDragTap then altDragTap:start() end
    hs.mouse.absolutePosition(savedCursor)
    isMovingWindowToSpace = false
  end

  local app = win:application()
  local bundleId = app and app:bundleID() or ""
  local winTitle = win:title() or ""
  if #winTitle == 0 and app then winTitle = app:title() or "" end
  local winCenter = hs.geometry(win:frame()).center
  local scr = win:screen() or hs.screen.mainScreen()
  local full = scr:fullFrame()
  local activeSpace = hs.spaces.activeSpaceOnScreen(scr)
  local spaceGlobalIdx = getGlobalSpaceIndex(targetSpaceId)
  if not spaceGlobalIdx then
    cleanupAndFinish()
    return
  end

  win:focus()
  hs.spaces.openMissionControl()
  postRawMouseMove({ x = math.floor(full.x + full.w / 2), y = math.floor(full.y + 20) })

  local waitAttempts = 0
  local pollTimer = nil
  pollTimer = hs.timer.doEvery(0.04, function()
    waitAttempts = waitAttempts + 1
    local mcGroup = getMissionControlGroup()
    if not mcGroup then
      if waitAttempts >= 25 then
        pollTimer:stop()
        pcall(hs.spaces.closeMissionControl)
        cleanupAndFinish()
      end
      return
    end
    pollTimer:stop()

    -- Allow Mission Control thumbnail layout & top Spaces bar expansion to settle
    hs.timer.doAfter(hs.spaces.MCwaitTime or 0.3, function()
      local mcWins, isWM = getMissionControlWindows()
      local mcSpaces = getMissionControlSpaces()
      local targetMcSpace = mcSpaces and mcSpaces[spaceGlobalIdx]
      if not mcWins or not targetMcSpace or not targetMcSpace.AXFrame then
        pcall(hs.spaces.closeMissionControl)
        cleanupAndFinish()
        return
      end

      local bestThumb = nil
      local bestDist = math.huge
      for _, cand in ipairs(mcWins) do
        local ident = cand.AXIdentifier or ""
        local candSpace = tonumber(ident:match("%.space%.(%d+)$"))
        local sameSpace = (not candSpace) or (candSpace == activeSpace)
        local sameApp = (not candSpace) or (bundleId ~= "" and ident:sub(1, #bundleId + 7) == bundleId .. ".space.")
        if sameSpace and sameApp and cand.AXFrame then
          local cCenter = hs.geometry(cand.AXFrame).center
          local dist = hs.geometry(cCenter):distance(winCenter)
          if mcTitleMatches(cand.AXTitle, winTitle) then
            dist = dist - 100000
          end
          if dist < bestDist then
            bestDist = dist
            bestThumb = cand
          end
        end
      end

      if not bestThumb or not bestThumb.AXFrame then
        pcall(hs.spaces.closeMissionControl)
        cleanupAndFinish()
        return
      end

      local startPt = hs.geometry(bestThumb.AXFrame).center
      local endPt = isWM and targetMcSpace.AXPosition or hs.geometry(targetMcSpace.AXFrame).center
      if not endPt then
        endPt = hs.geometry(targetMcSpace.AXFrame).center
      end

      local evNum = allocMouseEventNumber()
      postRawMouseMove(startPt)
      hs.timer.doAfter(0.02, function()
        postRawMouseButton(hs.eventtap.event.types.leftMouseDown, startPt, evNum, 0, 0)
        local steps = 6
        local stepIdx = 0
        local dragTimer = nil
        local prevPt = { x = startPt.x, y = startPt.y }
        dragTimer = hs.timer.doEvery(0.015, function()
          stepIdx = stepIdx + 1
          local frac = stepIdx / steps
          local curPt = {
            x = math.floor(startPt.x + (endPt.x - startPt.x) * frac),
            y = math.floor(startPt.y + (endPt.y - startPt.y) * frac),
          }
          postRawMouseButton(
            hs.eventtap.event.types.leftMouseDragged,
            curPt,
            evNum,
            curPt.x - prevPt.x,
            curPt.y - prevPt.y
          )
          prevPt = curPt
          if stepIdx >= steps then
            dragTimer:stop()
            hs.timer.doAfter(0.03, function()
              postRawMouseButton(hs.eventtap.event.types.leftMouseUp, endPt, evNum, 0, 0)
              hs.timer.doAfter(0.03, function()
                local switched = false
                if switchToSpace then
                  local freshSpaces = getMissionControlSpaces()
                  local spEl = freshSpaces and freshSpaces[spaceGlobalIdx]
                  if spEl then
                    switched = spEl:performAction("AXPress") or false
                  end
                end
                if not switched then
                  pcall(hs.spaces.closeMissionControl)
                end
                hs.timer.doAfter(0.25, function()
                  win:focus()
                  cleanupAndFinish()
                end)
              end)
            end)
          end
        end)
      end)
    end)
  end)
end

-- Fast titlebar-drag + Space-switch path for standard titled windows, with
-- automatic fallback to moveWindowViaMissionControl for borderless windows
-- (e.g. Kitty) or if macOS Ctrl+Arrow shortcuts are disabled.
local function moveFocusedWindowToTargetSpace(win, targetSpace, direction)
  if isMovingWindowToSpace or not win then return end
  if win:isFullScreen() then return end

  -- First attempt native API (works on macOS < 14.5)
  pcall(hs.spaces.moveWindowToSpace, win:id(), targetSpace)
  local currentWinSpaces = hs.spaces.windowSpaces(win)
  if currentWinSpaces and currentWinSpaces[1] == targetSpace then
    hs.spaces.gotoSpace(targetSpace)
    win:focus()
    return
  end

  local zoomRect = win:zoomButtonRect()
  local hasTitlebar = zoomRect and zoomRect.w and zoomRect.w > 0
  if not hasTitlebar or (direction ~= "left" and direction ~= "right") then
    moveWindowViaMissionControl(win, targetSpace, true)
    return
  end

  isMovingWindowToSpace = true
  local savedCursor = hs.mouse.absolutePosition()
  local origFrame = win:frame()
  local clickPos = {
    x = math.min(origFrame.x + origFrame.w - 12, math.floor(zoomRect.x + zoomRect.w + 10)),
    y = math.floor(zoomRect.y + zoomRect.h / 2),
  }
  local evNum = allocMouseEventNumber()
  local wasDragTapEnabled = altDragTap and altDragTap:isEnabled()
  if wasDragTapEnabled then altDragTap:stop() end

  win:unminimize()
  win:raise()
  postRawMouseMove(clickPos)

  hs.timer.doAfter(0.02, function()
    postRawMouseButton(hs.eventtap.event.types.leftMouseDown, clickPos, evNum, 0, 0)
    hs.timer.doAfter(0.03, function()
      local dragPt = { x = clickPos.x + 2, y = clickPos.y }
      postRawMouseButton(hs.eventtap.event.types.leftMouseDragged, dragPt, evNum, 2, 0)
      hs.timer.doAfter(0.04, function()
        -- Release held Alt/Shift modifiers so macOS sees pure Ctrl+Left/Right
        hs.eventtap.event.newKeyEvent("alt", false):post()
        hs.eventtap.event.newKeyEvent("shift", false):post()
        hs.eventtap.event.newKeyEvent("ctrl", true):post()
        hs.timer.doAfter(0.02, function()
          hs.eventtap.event.newKeyEvent(direction, true):setFlags({ctrl = true, fn = true}):post()
          hs.timer.doAfter(0.02, function()
            hs.eventtap.event.newKeyEvent(direction, false):setFlags({ctrl = true, fn = true}):post()
            hs.eventtap.event.newKeyEvent("ctrl", false):post()
            hs.timer.doAfter(0.45, function()
              postRawMouseButton(hs.eventtap.event.types.leftMouseUp, hs.mouse.absolutePosition(), evNum, 0, 0)
              hs.timer.doAfter(0.02, function()
                if win:isVisible() then
                  win:setFrame(origFrame)
                end
                win:raise()
                win:focus()
                hs.mouse.absolutePosition(savedCursor)
                if wasDragTapEnabled and altDragTap then altDragTap:start() end
                isMovingWindowToSpace = false

                -- Verify window reached targetSpace; if not, fall back to Mission Control drag
                local afterSpaces = hs.spaces.windowSpaces(win)
                if not (afterSpaces and afterSpaces[1] == targetSpace) then
                  moveWindowViaMissionControl(win, targetSpace, true)
                end
              end)
            end)
          end)
        end)
      end)
    end)
  end)
end

local function switchToWorkspaceIndex(idx)
  local scr = hs.screen.mainScreen()
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  if #spaces < idx and idx <= 6 then
    spaces = ensureUserSpacesCount(scr, idx)
  end
  local targetSpace = spaces[idx]
  if targetSpace then
    hs.spaces.gotoSpace(targetSpace)
  end
end

local function moveFocusedWindowToWorkspaceIndex(idx)
  local win = hs.window.focusedWindow()
  if not win then return end
  local scr = win:screen() or hs.screen.mainScreen()
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  if #spaces < idx and idx <= 6 then
    spaces = ensureUserSpacesCount(scr, idx)
  end
  local targetSpace = spaces[idx]
  if not targetSpace then return end
  local currentSpace = hs.spaces.activeSpaceOnScreen(scr) or hs.spaces.focusedSpace()
  if targetSpace == currentSpace then return end
  moveFocusedWindowToTargetSpace(win, targetSpace, nil)
end

local function moveFocusedWindowToAdjacentSpace(delta)
  local win = hs.window.focusedWindow()
  if not win then return end
  local scr = win:screen() or hs.screen.mainScreen()
  local spaces = getOrderedUserSpacesForCurrentScreen(scr)
  if #spaces < 2 then
    spaces = ensureUserSpacesCount(scr, 6)
  end
  local currentSpace = hs.spaces.activeSpaceOnScreen(scr) or hs.spaces.focusedSpace()
  local currentIdx = nil
  for i, spId in ipairs(spaces) do
    if spId == currentSpace then
      currentIdx = i
      break
    end
  end
  if not currentIdx then return end
  local targetIdx = currentIdx + delta
  if targetIdx > #spaces and targetIdx <= 6 then
    spaces = ensureUserSpacesCount(scr, targetIdx)
  end
  if targetIdx >= 1 and targetIdx <= #spaces then
    local targetSpace = spaces[targetIdx]
    local dir = (delta > 0) and "right" or "left"
    moveFocusedWindowToTargetSpace(win, targetSpace, dir)
  else
    hs.alert.show(string.format("Already at Space %d of %d", currentIdx, #spaces), 0.7)
  end
end

for i = 1, 6 do
  hs.hotkey.bind({"ctrl", "alt"}, tostring(i), function()
    switchToWorkspaceIndex(i)
  end)
  hs.hotkey.bind({"ctrl", "shift", "alt"}, tostring(i), function()
    moveFocusedWindowToWorkspaceIndex(i)
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
-- Replicates ~/modules/x-window-shortcuts/rofi-window-switcher and
-- fzf-window-switcher.sh:
--   - Tracks windows across all workspaces (Spaces 1..6) in MRU order.
--   - Formats entries as: "<workspace_num> <app_prefix>: <title> (0x<win_id>)".
--   - Opens with default prefix filter "^<current_workspace_num> " so only
--     current-workspace windows appear initially (with PREV_WINDOW_ID at row 1).
--   - Deleting the "^<N> " prefix filter (via Backspace, Ctrl+U, Ctrl+W,
--     Alt+Backspace, or Cmd+Backspace) reveals all windows across all
--     workspaces (1..6).
--   - Selecting a window on another workspace switches focus to that workspace
--     and raises + focuses the target window.
local mruWindowIds = hs.settings.get("hammerspoonMruIds") or {}
local knownWindowsById = {}
local persistedWinMeta = hs.settings.get("hammerspoonWindowCache") or {}
for k, v in pairs(persistedWinMeta) do
  local numId = tonumber(k)
  if numId and type(v) == "table" then
    knownWindowsById[numId] = v
  end
end

local highlightCanvas = nil
local windowChooser = nil
local chooserPollTimer = nil
local chooserKeyTap = nil
local lastSelectedRow = nil
local allChooserChoices = {}
local initialChooserQuery = ""
local isInitialPrefixActive = false

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

local function persistWindowTrackerState()
  local serializableMeta = {}
  for winId, meta in pairs(knownWindowsById) do
    serializableMeta[tostring(winId)] = {
      winId = winId,
      title = meta.title or "",
      appName = meta.appName or "Unknown",
      bundleID = meta.bundleID,
      pid = meta.pid or 0,
      spaceId = meta.spaceId,
    }
  end
  pcall(hs.settings.set, "hammerspoonWindowCache", serializableMeta)
  pcall(hs.settings.set, "hammerspoonMruIds", mruWindowIds)
end

local function recordWindowMetadata(win, explicitSpaceId)
  if not win then return end
  local okId, winId = pcall(function() return win:id() end)
  if not okId or not winId or winId <= 0 then return end
  local okStd, isStd = pcall(function() return win:isStandard() end)
  if okStd and not isStd then return end

  local app = win:application()
  local appName = app and app:name() or "Unknown"
  if IGNORED_APP_NAMES[appName] then return end
  if appName == "Hammerspoon" and (win:title() or "") == "" then return end

  local bundleID = app and app:bundleID() or nil
  local pid = app and app:pid() or 0
  local title = win:title() or ""
  local winSpaces = hs.spaces.windowSpaces(winId)
  local spId = explicitSpaceId or (winSpaces and winSpaces[1]) or
               (knownWindowsById[winId] and knownWindowsById[winId].spaceId) or
               hs.spaces.focusedSpace()

  knownWindowsById[winId] = {
    win = win,
    winId = winId,
    title = title,
    appName = appName,
    bundleID = bundleID,
    pid = pid,
    spaceId = spId,
  }
end

local function scanActiveSpaceWindows()
  local activeSpace = hs.spaces.focusedSpace()
  for _, w in ipairs(hs.window.orderedWindows()) do
    recordWindowMetadata(w, activeSpace)
  end
  persistWindowTrackerState()
end

local wf = hs.window.filter.new()
wf:subscribe({
  hs.window.filter.windowFocused,
  hs.window.filter.windowCreated,
  hs.window.filter.windowTitleChanged,
  hs.window.filter.windowMoved,
}, function(win, _, event)
  if not win then return end
  recordWindowMetadata(win, nil)
  local okId, id = pcall(function() return win:id() end)
  if not okId or not id then return end
  if event == hs.window.filter.windowFocused then
    for i = #mruWindowIds, 1, -1 do
      if mruWindowIds[i] == id then
        table.remove(mruWindowIds, i)
      end
    end
    table.insert(mruWindowIds, 1, id)
    while #mruWindowIds > 64 do
      table.remove(mruWindowIds)
    end
  end
  persistWindowTrackerState()
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
  persistWindowTrackerState()
end)

-- Also refresh active Space window metadata whenever Spaces switch
local mruSpaceWatcher = hs.spaces.watcher.new(function(_)
  hs.timer.doAfter(0.1, scanActiveSpaceWindows)
end)
mruSpaceWatcher:start()
hs.timer.doAfter(0.2, scanActiveSpaceWindows)

local function clearWindowBorder()
  if chooserPollTimer then
    chooserPollTimer:stop()
    chooserPollTimer = nil
  end
  if chooserKeyTap then
    chooserKeyTap:stop()
    chooserKeyTap = nil
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

local function getSpaceIndexMaps()
  local spaceIndexById = {}
  local spaceIdByIndex = {}
  local mainScr = hs.screen.mainScreen()
  local mainSpaces = getOrderedUserSpacesForCurrentScreen(mainScr)
  for idx, spId in ipairs(mainSpaces) do
    spaceIndexById[spId] = idx
    spaceIdByIndex[idx] = spId
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
  local focusedSpaceId = hs.spaces.focusedSpace()
  local currentSpaceIdx = (focusedSpaceId and spaceIndexById[focusedSpaceId]) or 1
  return spaceIndexById, spaceIdByIndex, focusedSpaceId, currentSpaceIdx
end

local function buildAllWorkspacesChooserChoices()
  local spaceIndexById, _, focusedSpaceId, currentSpaceIdx = getSpaceIndexMaps()

  -- 1. Refresh windows on the currently active Space
  local activeOrdered = hs.window.orderedWindows()
  local activeOrderRank = {}
  for rank, w in ipairs(activeOrdered) do
    if w:isStandard() and w:isVisible() and w:id() then
      recordWindowMetadata(w, focusedSpaceId)
      activeOrderRank[w:id()] = rank
    end
  end

  -- 2. Ingest windows tracked across all Spaces by hs.window.filter
  local okWf, wfWins = pcall(function()
    return wf:getWindows(hs.window.filter.sortByFocusedLast)
  end)
  if okWf and type(wfWins) == "table" then
    for _, w in ipairs(wfWins) do
      recordWindowMetadata(w, nil)
    end
  end

  -- 3. Map every Space's window IDs via hs.spaces.windowsForSpace and optional
  --    CoreGraphics WindowServer list (hs.window.list) for unvisited Spaces
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

  local cgInfoById = {}
  if type(hs.window.list) == "function" then
    local okCg, cgList = pcall(hs.window.list, true)
    if okCg and type(cgList) == "table" then
      for _, info in ipairs(cgList) do
        local wid = info.kCGWindowNumber
        if wid and (info.kCGWindowLayer or 0) == 0 then
          cgInfoById[wid] = info
        end
      end
    end
  end

  for wid, spId in pairs(spaceByWinId) do
    if not knownWindowsById[wid] and cgInfoById[wid] then
      local info = cgInfoById[wid]
      local bounds = info.kCGWindowBounds or {}
      local wWidth = bounds.Width or bounds.w or 0
      local wHeight = bounds.Height or bounds.h or 0
      local ownerName = info.kCGWindowOwnerName or ""
      local pid = info.kCGWindowOwnerPID or 0
      local app = (pid > 0) and hs.application.applicationForPID(pid) or nil
      local isRegularApp = app and (app:kind() == 1)
      if isRegularApp and not IGNORED_APP_NAMES[ownerName] and wWidth >= 100 and wHeight >= 80 then
        if not (ownerName == "Hammerspoon" and (info.kCGWindowName or "") == "") then
          knownWindowsById[wid] = {
            win = hs.window.get(wid),
            winId = wid,
            title = info.kCGWindowName or ownerName,
            appName = app:name() or ownerName,
            bundleID = app:bundleID(),
            pid = pid,
            spaceId = spId,
          }
        end
      end
    end
  end

  -- 4. Prune closed windows and determine each window's workspace index (1..6)
  local validEntriesById = {}
  local candidateIds = {}
  for wid, meta in pairs(knownWindowsById) do
    local liveWin = hs.window.get(wid) or meta.win
    local winSpaces = hs.spaces.windowSpaces(wid)
    local spId = spaceByWinId[wid] or (winSpaces and winSpaces[1]) or meta.spaceId
    local stillAlive = (activeOrderRank[wid] ~= nil) or
                       (spaceByWinId[wid] ~= nil) or
                       (winSpaces and #winSpaces > 0)
    if stillAlive and spId then
      if liveWin then
        local okT, liveTitle = pcall(function() return liveWin:title() end)
        if okT and liveTitle and liveTitle ~= "" then
          meta.title = liveTitle
        end
      end
      local isSticky = (stickyWindows[wid] == true) or
                       ((winSpaces and #winSpaces > 1) or (spaceCountByWinId[wid] or 0) > 1)
      local rawSpaceIdx = spaceIndexById[spId] or currentSpaceIdx
      -- Sticky windows (-1 in wmctrl) match current workspace filter just like rofi-window-switcher
      local displaySpaceIdx = isSticky and currentSpaceIdx or rawSpaceIdx
      meta.spaceId = spId
      meta.spaceIdx = rawSpaceIdx
      meta.displaySpaceIdx = displaySpaceIdx
      meta.isSticky = isSticky
      meta.win = liveWin
      validEntriesById[wid] = meta
      table.insert(candidateIds, wid)
    else
      knownWindowsById[wid] = nil
    end
  end

  -- 5. Sort candidates in MRU order, placing PREV_WINDOW_ID on the current
  --    workspace first so <Alt>Tab -> Enter immediately switches to the
  --    previous window on the current workspace (matching rofi-window-switcher).
  local focusedWin = hs.window.focusedWindow()
  local focusedWinId = focusedWin and focusedWin:id() or mruWindowIds[1]

  local prevOnCurrentSpaceId = nil
  for _, id in ipairs(mruWindowIds) do
    local entry = validEntriesById[id]
    if entry and id ~= focusedWinId and entry.displaySpaceIdx == currentSpaceIdx then
      prevOnCurrentSpaceId = id
      break
    end
  end
  if not prevOnCurrentSpaceId then
    for _, w in ipairs(activeOrdered) do
      local wid = w:id()
      if wid and wid ~= focusedWinId and validEntriesById[wid] then
        prevOnCurrentSpaceId = wid
        break
      end
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

  -- First: previous window on current workspace (row 1 when "^<N> " is active)
  pushId(prevOnCurrentSpaceId)
  -- Second: global previous window if on another workspace (row 1 when "^<N> " is cleared)
  if mruWindowIds[2] and mruWindowIds[2] ~= focusedWinId then
    pushId(mruWindowIds[2])
  end
  -- Third: all remaining MRU windows
  for _, id in ipairs(mruWindowIds) do
    pushId(id)
  end
  -- Fourth: any remaining windows on active Space in z-order
  for _, w in ipairs(activeOrdered) do
    pushId(w:id())
  end
  -- Fifth: any remaining windows on other Spaces sorted by workspace number
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

  -- 6. Build hs.chooser items formatted like rofi-window-switcher / fzf-window-switcher.sh
  local choices = {}
  for _, wid in ipairs(orderedIds) do
    local meta = validEntriesById[wid]
    local appName = meta.appName or "Unknown"
    local title = meta.title ~= "" and meta.title or appName
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
    local icon = meta.bundleID and hs.image.imageFromAppBundle(meta.bundleID) or nil

    table.insert(choices, {
      text = entryText,
      subText = subText,
      image = icon,
      winId = wid,
      spaceId = meta.spaceId,
      spaceIdx = meta.spaceIdx,
      displaySpaceIdx = meta.displaySpaceIdx,
      isSticky = meta.isSticky,
      bundleID = meta.bundleID,
      appName = appName,
    })
  end

  return choices, currentSpaceIdx, focusedSpaceId
end

local function choiceMatchesQuery(choice, rawQuery)
  if not rawQuery or rawQuery == "" or rawQuery == "^" then
    return true
  end

  local q = rawQuery
  -- Match leading workspace regex filter "^<workspace_num>" (e.g. "^1 " or "^1 kitty")
  local wsPrefix, remainder = q:match("^%^([0-9]+)(.*)")
  if wsPrefix then
    local targetWs = tonumber(wsPrefix)
    if choice.displaySpaceIdx ~= targetWs then
      return false
    end
    q = remainder or ""
  end

  q = q:match("^%s*(.-)%s*$") or ""
  if q == "" or q == "^" then
    return true
  end

  local haystack = string.lower((choice.text or "") .. " " .. (choice.subText or ""))
  local textLower = string.lower(choice.text or "")

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

local function updateSelectedRowBorder()
  if not windowChooser or not windowChooser:isVisible() then return end
  local row = windowChooser:selectedRow()
  local item = row and windowChooser:selectedRowContents(row) or nil
  local activeSpace = hs.spaces.focusedSpace()
  if item and item.winId and (item.isSticky or item.spaceId == activeSpace) then
    drawBlueBorderAroundWindow(hs.window.get(item.winId))
  else
    if highlightCanvas then
      highlightCanvas:delete()
      highlightCanvas = nil
    end
  end
end

local function focusSelectedWindowChoice(choice)
  if not choice or not choice.winId then return end
  local winId = choice.winId
  local targetSpaceId = choice.spaceId
  local targetSpaceIdx = choice.spaceIdx
  local currentSpaceId = hs.spaces.focusedSpace()

  local function raiseAndFocusTarget()
    local w = hs.window.get(winId) or (knownWindowsById[winId] and knownWindowsById[winId].win)
    if w then
      pcall(function()
        if w:isMinimized() then w:unminimize() end
        local app = w:application()
        if app then app:activate() end
        w:raise()
        w:focus()
      end)
      return true
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
  if choice.isSticky or not targetSpaceId or targetSpaceId == currentSpaceId then
    raiseAndFocusTarget()
    return
  end

  -- Different workspace: switch to the target workspace first, then raise & focus window
  if targetSpaceIdx and targetSpaceIdx >= 1 and targetSpaceIdx <= 6 then
    switchToWorkspaceIndex(targetSpaceIdx)
  else
    pcall(hs.spaces.gotoSpace, targetSpaceId)
  end

  local attempts = 0
  local focusTimer = nil
  focusTimer = hs.timer.doEvery(0.05, function()
    attempts = attempts + 1
    local nowSpace = hs.spaces.focusedSpace()
    if nowSpace == targetSpaceId or attempts >= 12 then
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
  -- If the user presses Backspace once on the initial "^<N> " prefix, clear the
  -- entire prefix immediately so all workspaces (1..6) are shown in 1 keystroke.
  if isInitialPrefixActive then
    if newQuery == initialChooserQuery:sub(1, -2) then
      isInitialPrefixActive = false
      windowChooser:query("")
      return
    elseif newQuery ~= initialChooserQuery then
      isInitialPrefixActive = false
    end
  end

  local filtered = filterChooserChoices(allChooserChoices, newQuery)
  windowChooser:choices(filtered)
  lastSelectedRow = nil
  updateSelectedRowBorder()
end)

local function moveChooserCaretToEnd(queryLen)
  pcall(function()
    local hsApp = hs.application.get(hs.processInfo.processID)
    if not hsApp then return end
    local axApp = hs.axuielement.applicationElement(hsApp)
    local axWin = axApp and axApp:attributeValue("AXFocusedWindow")
    if not axWin then return end
    local children = axWin:attributeValue("AXChildren") or {}
    for _, child in ipairs(children) do
      if child:attributeValue("AXRole") == "AXTextField" then
        child:setAttributeValue("AXSelectedTextRange", {location = queryLen, length = 0})
        break
      end
    end
  end)
end

hs.hotkey.bind({"alt"}, "tab", function()
  local choices, currentSpaceIdx = buildAllWorkspacesChooserChoices()
  allChooserChoices = choices
  initialChooserQuery = string.format("^%d ", currentSpaceIdx)
  isInitialPrefixActive = true

  windowChooser:choices(filterChooserChoices(allChooserChoices, initialChooserQuery))
  windowChooser:query(initialChooserQuery)
  windowChooser:show()

  -- Place caret at the end of "^<N> " (matching rofi -filter "^<N> " / fzf --query "^<N> ")
  hs.timer.doAfter(0.01, function()
    moveChooserCaretToEnd(#initialChooserQuery)
    updateSelectedRowBorder()
  end)

  -- Support Ctrl+U / Ctrl+W / Alt+Backspace inside chooser to clear prefix in 1 keystroke
  if chooserKeyTap then chooserKeyTap:stop() end
  chooserKeyTap = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(e)
    if not windowChooser:isVisible() then return false end
    local flags = e:getFlags()
    local keyCode = e:getKeyCode()
    local ch = string.lower(e:getCharacters(true) or "")
    -- Ctrl+U (clear line) or Ctrl+W (backward kill word) or Alt+Backspace (keyCode 51)
    if (flags.ctrl and not flags.cmd and not flags.alt and (ch == "u" or ch == "w")) or
       (flags.alt and not flags.cmd and not flags.ctrl and keyCode == 51) then
      isInitialPrefixActive = false
      windowChooser:query("")
      return true
    end
    return false
  end)
  chooserKeyTap:start()

  lastSelectedRow = nil
  chooserPollTimer = hs.timer.doEvery(0.04, function()
    if not windowChooser:isVisible() then
      clearWindowBorder()
      return
    end
    local row = windowChooser:selectedRow()
    if row ~= lastSelectedRow then
      lastSelectedRow = row
      updateSelectedRowBorder()
    end
  end)
end)

-- -----------------------------------------------------------------------------
-- 7. Zen Mode (<WM_MOD>z), Terminal-Scoped Notes (<WM_MOD>n), & App Shortcuts
-- -----------------------------------------------------------------------------
hs.hotkey.bind(WM_MOD, "z", function()
  hs.task.new("/bin/bash", nil, {os.getenv("HOME") .. "/scripts/zen-mode.sh"}):start()
end)

-- Terminal-scoped Notes launcher (replicates `if-terminal-has-focus ...`)
hs.hotkey.bind(WM_MOD, "n", function()
  local frontApp = hs.application.frontmostApplication()
  local name = frontApp and string.lower(frontApp:name() or "") or ""
  if name == "kitty" or name == "wezterm" or name == "iterm2" or name == "terminal" then
    hs.task.new("/bin/bash", nil, {"-lc", os.getenv("HOME") .. "/.googlerc.d/bin/kitty-ssh-launch tab"}):start()
  end
end)

-- Launchers matching xfce4-keyboard-shortcuts.xml
hs.hotkey.bind({"ctrl", "alt"}, "t", function()
  hs.application.launchOrFocus("kitty")
end)

hs.hotkey.bind({"ctrl", "alt"}, "l", function()
  hs.caffeinate.lockScreen()
end)
