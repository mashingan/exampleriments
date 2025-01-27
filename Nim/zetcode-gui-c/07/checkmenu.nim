import std/[winlean]
import ../basic_windows_05

const
  idmFileNew = 1
  idmFileOpen = 2
  idmFileQuit = 3
  idmViewStb = 1

template L(str: string): WideCString = newWideCString(str)

var gmenu, ghsb: Handle

proc addMenus(hwnd: Handle) =
  var menubar = createMenu()
  gmenu = createMenu()
  discard appendMenuW(gmenu, MF_STRING, idmViewStb, L"&Statusbar")
  checkMenuItem(gmenu, idmViewStb, MF_CHECKED)
  discard appendMenuW(menubar, MF_POPUP, cast[uint](gmenu), L"&View")
  setMenu(hwnd, menubar)

proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    addMenus(hwnd)
    initCommonControls()
    ghsb = createWindowW(statusClassNameW, L"", WS_CHILD or WS_VISIBLE,
                         0, 0, 0, 0, hwnd, 1, getModuleHandle(""), nil)
  of WM_COMMAND:
    case loword(wparam)
    of idmViewStb:
      let state = getMenuState(gmenu, idmViewStb, MF_BYCOMMAND)
      if (state == MF_CHECKED):
        showWindow(ghsb, SW_HIDE)
        checkMenuItem(gmenu, idmViewStb, MF_UNCHECKED)
      else:
        showWindow(ghsb, SW_SHOW)
        checkMenuItem(gmenu, idmViewStb, MF_CHECKED)
    else:
      discard
  of WM_SIZE:
    discard sendMessage(ghsb, WM_SIZE, wparam, lparam)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Check menu item"
sampleWindow name, name, wndProc
