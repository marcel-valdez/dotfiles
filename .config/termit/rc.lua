-- Example file with lots of options.
-- You can test with with this command:
-- cd ./etc && ../src/termit --init ../doc/rc.lua.example

colormaps = require("termit.colormaps")
utils = require("termit.utils")

defaults = {}
defaults.windowTitle = 'terminal'
defaults.startMaximized = true
defaults.hideTitlebarWhenMaximized = true
defaults.tabName = 'terminal'
defaults.encoding = 'UTF-8'
defaults.wordCharExceptions = '- .,_/'
defaults.font = 'Source Code Pro for Powerline 12'
--defaults.foregroundColor = 'gray'
--defaults.backgroundColor = 'black'
defaults.showScrollbar = false
defaults.hideSingleTab = false
defaults.hideTabbar = false
defaults.showBorder = false
defaults.hideMenubar = true
defaults.fillTabbar = false
defaults.scrollbackLines = 10000
defaults.geometry = '80x24'
defaults.allowChangingTitle = false
--defaults.backspaceBinding = 'AsciiBksp'
--defaults.deleteBinding = 'AsciiDel'
defaults.cursorBlinkMode = 'BlinkOn'
defaults.cursorShape = 'Ibeam'
defaults.tabPos = 'Top'
defaults.setStatusbar = function (tabInd)
    tab = tabs[tabInd]
    if tab then
        return tab.encoding..'  Bksp: '..tab.backspaceBinding..'  Del: '..tab.deleteBinding
    end
    return ''
end
defaults.colormap = colormaps.zenburn
defaults.matches = {['http[-:/.\\w]+'] = function (url) print('Matching url: '..url) end}
--defaults.tabs = {{title = 'Test new tab 1'; workingDir = '/tmp'};
--    {title = 'Test new tab 2'; workingDir = '/tmp'}}
setOptions(defaults)

bindKey('CtrlShift-Page_Up', prevTab)
bindKey('CtrlShift-Page_Down', nextTab)
bindKey('CtrlShift-F', findDlg)
--bindKey('Ctrl-2', function () print('Hello2!') end)
--bindKey('Ctrl-3', function () print('Hello3!') end)
--bindKey('Ctrl-3', nil) -- remove previous binding

-- don't close tab with Ctrl-w, use Ctrl-F4
bindKey('Ctrl-w', nil)
bindKey('Ctrl-F4', closeTab)
bindKey('Ctrl-F2', setTabTitleDlg)
bindKey('Alt-Right', nil)
bindKey('Alt-Left', nil)
bindKey('Ctrl-t', nil)
bindKey('Super-t', openTab)
bindKey('Super-r', reconfigure)
bindKey('CtrlShift-v', paste)
bindKey('CtrlShift-c', copy)

setKbPolicy('keycode')

-- bindMouse('DoubleClick', openTab)
userMenu = {}
table.insert(userMenu, {name='Close tab', action=closeTab, accel='x'})
--mi = {}
--mi.name = 'Zsh tab'
--mi.action = function ()
--    tabInfo = {}
--    tabInfo.title = 'Zsh tab'
--    tabInfo.command = 'zsh'
--    tabInfo.encoding = 'UTF-8'
--    tabInfo.workingDir = '/tmp'
--    tabInfo.backspaceBinding = 'AsciiBksp'
--    tabInfo.deleteBinding = 'EraseDel'
--    openTab(tabInfo)
--end
--table.insert(userMenu, mi)

--table.insert(userMenu, {name='set red color', action=function () setTabForegroundColor('red') end})
table.insert(userMenu, {name='Reconfigure', action=reconfigure, accel='c'})
table.insert(userMenu, {name='Selection', action=function () print(selection()) end})
table.insert(userMenu, {name='Dump All Rows', action=function () forEachRow(print) end})
table.insert(userMenu, {name='Dump Visible Rows To File',
    action=function () utils.dumpToFile(forEachVisibleRow, '/tmp/termit.dump') end})
table.insert(userMenu, {name='Find Next', action=findNext, accel='n'})
table.insert(userMenu, {name='Find Prev', action=findPrev, accel='p'})
table.insert(userMenu, {name='Toggle Menubar', action=function () toggleMenubar() end, accel='m'})
table.insert(userMenu, {name='Toggle Tab Bar', action=function () toggleTabbar()  end, accel='t'})

--mi = {}
--mi.name = 'Get tab info'
--mi.action = function ()
--    tab = tabs[currentTabIndex()]
--    if tab then
--        utils.printTable(tab, '  ')
--    end
--end
--table.insert(userMenu, mi)

function round(float)
    return math.floor(float + .5)
end

function changeTabFontSize(delta)
    tab = tabs[currentTabIndex()]
    fontSize = round(tab.fontSize)
    setTabFont(string.sub(tab.font, 1, string.find(tab.font, '%d+$') - 1)..(fontSize + delta))
end

function increaseTabFontSize()
   changeTabFontSize(1)
end

bindKey('Super-equal', increaseTabFontSize)
table.insert(userMenu, {
                name='Increase font size',
                action=increaseTabFontSize,
                accel='='
})

function decreaseTabFontSize()
   changeTabFontSize(-1)
end

bindKey('Super-minus', decreaseTabFontSize)
table.insert(userMenu, {
                name='Decrease font size',
                action=decreaseTabFontSize,
                accel='-'
})
--table.insert(userMenu, {name='feed example', action=function () feed('example') end})
--table.insert(userMenu, {name='feedChild example', action=function () feedChild('date\n') end})
function moveTabLeft ()
   setTabPos(currentTabIndex() - 1)
end
bindKey('CtrlSuper-Page_Up', moveTabLeft)

function moveTabRight ()
   setTabPos(currentTabIndex() + 1)
end
bindKey('CtrlSuper-Page_Down', moveTabRight)
table.insert(userMenu, { name='Move Tab Left', action=moveTabLeft, accel='l' })
table.insert(userMenu, { name='Move Tab Right', action=moveTabRight, accel='r' })
table.insert(userMenu, { name='Quit', action=quit, accel='q' })

addMenu(userMenu, "User Menu")
addPopupMenu(userMenu, "User Menu")

addMenu(utils.encMenu(), "Encodings")
addPopupMenu(utils.encMenu(), "Encodings")

colorMapMenu = {}
for colormap_name, colormap_colors in pairs(colormaps) do
   table.insert(
      colorMapMenu,
      { name=colormap_name, action=function () setColormap(colormap_colors) end }
   )
end
addMenu(colorMapMenu, "Set Colormap")
addPopupMenu(colorMapMenu, "Set Colormap")
