import std/[winlean]
import ../basic_windows_05

const
  idmFileNew = 1
  idmFileImport = 2
  idmImportMail = 11

template L(str: string): WideCString = newWideCString(str)

proc addMenus(hwnd: Handle) =
  var
    menubar = createMenu()
    menu = createMenu()
    submenu = createPopupMenu()
  discard appendMenuW(menu, MF_STRING, idmFileNew, L"&New")
  discard appendMenuW(menu, MF_STRING or MF_POPUP, cast[uint](submenu), L"&Import")
  discard appendMenuW(submenu, MF_STRING, idmImportMail, L"Import &mail")
  discard appendMenuW(menubar, MF_POPUP, cast[uint](menu), L"&File")
  setMenu(hwnd, menubar)

proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CREATE:
    addMenus(hwnd)
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_COMMAND:
    let info = L"Information"
    case loword(wparam)
    of idmFileNew:
      discard messageBoxW(0, L"New file selected", info, MB_OK)
    of idmImportMail:
      discard messageBoxW(0, L"Import mail selected", info, MB_OK)
    else:
      discard
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Submenu"
sampleWindow name, name, wndProc
