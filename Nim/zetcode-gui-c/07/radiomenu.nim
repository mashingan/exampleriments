import std/[winlean]
import ../basic_windows_05

const
  idmModeMap = 1
  idmModeSat = 2
  idmModeTra = 3
  idmModeStr = 4

template L(str: string): WideCString = newWideCString(str)

proc addMenus(hwnd: Handle, menu: var Handle) =
  var menubar = createMenu()
  menu = createMenu()
  discard appendMenuW(menu, MF_STRING, idmModeMap, L"&Map")
  discard appendMenuW(menu, MF_STRING, idmModeSat, L"&Satellite")
  discard appendMenuW(menu, MF_STRING, idmModeTra, L"&Traffic")
  discard appendMenuW(menu, MF_STRING, idmModeStr, L"Street &view")
  discard checkMenuRadioItem(menu, idmModeMap, idmModeStr, idmModeMap, MF_BYCOMMAND)
  discard appendMenuW(menubar, MF_POPUP, cast[uint](menu), L"&Map mode")
  setMenu(hwnd, menubar)

var gmenu: Handle
proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    addMenus(hwnd, gmenu)
  of WM_COMMAND:
    case loword(wparam)
    of idmModeMap:
      discard checkMenuRadioItem(gmenu, idmModeMap, idmModeStr, idmModeMap, MF_BYCOMMAND)
      discard messageBeep(MB_ICONERROR)
    of idmModeSat:
      discard checkMenuRadioItem(gmenu, idmModeMap, idmModeStr, idmModeSat, MF_BYCOMMAND)
      discard messageBeep(0xffff)
    of idmModeTra:
      discard checkMenuRadioItem(gmenu, idmModeMap, idmModeStr, idmModeTra, MF_BYCOMMAND)
      discard messageBeep(MB_ICONWARNING)
    of idmModeStr:
      discard checkMenuRadioItem(gmenu, idmModeMap, idmModeStr, idmModeStr, MF_BYCOMMAND)
      discard messageBeep(MB_ICONINFORMATION)
    else:
      discard
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Radio check menu"
sampleWindow name, name, wndProc
