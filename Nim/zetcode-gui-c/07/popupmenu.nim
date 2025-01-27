import std/[winlean]
import ../basic_windows_05

const
  idmFileNew = 1
  idmFileOpen = 2
  idmFileQuit = 3
  idmViewStb = 1

template L(str: string): WideCString = newWideCString(str)

proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_COMMAND:
    case loword(wparam)
    of idmFileNew, idmFileOpen:
      discard messageBeep(MB_ICONINFORMATION)
    of idmFileQuit:
      discard sendMessage(hwnd, WM_QUIT, 0, 0)
    else:
      discard
  of WM_RBUTTONUP:
    var point = Point(
      x: clong loword(uint lparam),
      y: clong hiword(uint lparam),
    )
    var menu = createPopupMenu()
    clientToScreen(hwnd, addr point)
    discard appendMenuW(menu, MF_STRING, idmFileNew, L"&New")
    discard appendMenuW(menu, MF_STRING, idmFileOpen, L"&Open")
    discard appendMenuW(menu, MF_SEPARATOR, 0, L"")
    discard appendMenuW(menu, MF_STRING, idmFileQuit, L"&Quit")
    trackPopupMenu(menu, TPM_RIGHTBUTTON, point.x, point.y, 0, hwnd, nil)
    destroyMenu menu
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Popup menu"
sampleWindow name, name, wndProc
