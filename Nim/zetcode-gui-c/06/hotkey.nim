import std/winlean
import ../basic_windows_05
import ../../zmisc/kbdinputs

const idHotkey = 1

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
    registerHotKey(hwnd, idHotkey, MOD_CONTROL, keymap['c'])
  of WM_DESTROY, WM_QUIT, WM_CLOSE:
    unregisterHotKey(hwnd, idHotkey)
    postQuitMessage 0
  of WM_HOTKEY:
    if wparam == idHotkey:
      centerWindow(hwnd)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = newWideCString("Hotkey center window")
sampleWindow(name, name, wndProc)
