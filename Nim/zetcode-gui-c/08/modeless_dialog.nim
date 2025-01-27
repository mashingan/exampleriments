import std/[winlean, with]
import ../basic_windows_05

var
  ghinst: Hinstance

proc dialogCb(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CREATE:
    discard createWindowW(L"button", L"ok", WS_VISIBLE or WS_CHILD,
                          50, 50, 80, 25, hwnd, 1, nil, nil)
  of WM_COMMAND:
    destroyWindow hwnd
  of WM_CLOSE:
    destroyWindow hwnd
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

proc registerDialog(hwnd: Handle) =
  var wc = WndClassExW(
    size: uint32 sizeof(WndClassExW),
    wndProc: dialogCb,
    background: getSysColorBrush(COLOR_3DFACE),
    className: L"DialogClass",
    instance: ghinst,
  )
  registerClassExW(wc.addr)

proc createDialog(hwnd: Handle) =
  discard createWindowExW(WS_EX_DLGMODALFRAME or WS_EX_TOPMOST,
                          L"DialogClass", L"Dialog box",
                          WS_VISIBLE or WS_SYSMENU or WS_CAPTION,
                          100, 100, 200, 150, 0, 0, ghinst, nil)

proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_DESTROY, WM_CLOSE:
    postQuitMessage 0
  of WM_CREATE:
    registerDialog hwnd
    discard createWindowW(L"button", L"Show dialog", WS_VISIBLE or WS_CHILD,
                          20, 50, 95, 25, hwnd, 1, nil, nil)
  of WM_COMMAND:
    createDialog hwnd
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

ghinst = getModuleHandle ""
let name = L"Modeless dialog"
sampleWindow name, name, wndProc
