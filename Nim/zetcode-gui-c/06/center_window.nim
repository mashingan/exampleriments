import std/winlean
import ../basic_windows_05

proc centerWindow(hwnd: Handle) =
  var rc: Rect
  getWindowRect(hwnd, rc.addr)
  let
    w = rc.right - rc.left
    h = rc.bottom - rc.top
    screenw = getSystemMetrics(smCxScreen)
    screenh = getSystemMetrics(smCyScreen)
  setWindowPos(hwnd, HWND_TOP, (screenw - w) div 2, (screenh - h) div 2, 0, 0, SWP_NOSIZE)

proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CREATE:
    centerWindow(hwnd)
  of WM_DESTROY, WM_QUIT, WM_CLOSE:
    postQuitMessage 0
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = newWideCString("Center window")
sampleWindow(name, name, wndProc)
