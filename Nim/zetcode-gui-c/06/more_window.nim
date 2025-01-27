from std/winlean import Handle
from std/colors import colRed, colBlue
import ../basic_windows_05.nim

proc panelProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_LBUTTONUP:
    discard messageBeep(MB_OK)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

proc registerPanelClass(color: int32, classname: WideCString) =
  var brush = createSolidBrush(color)
  var wc = WndClassW(
    className: classname,
    background: brush,
    wndProc: panelProc,
    cursor: loadCursor(0, idcArrow),
  )
  registerClassW(addr wc)

proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CREATE:
    let redpanel = newWideCString "RedPanelClass"
    let bluepanel = newWideCString "BluePanelClass"
    registerPanelClass(colRed.int32, redpanel)
    discard createWindowW(redpanel, nil, WS_CHILD or WS_VISIBLE, 20, 20, 80, 80,
                          hwnd, 1, nil, nil)
    registerPanelClass(colBlue.int32, bluepanel)
    discard createWindowW(bluepanel, nil, WS_CHILD or WS_VISIBLE, 120, 20, 80, 80,
                          hwnd, 2, nil, nil)
  of WM_DESTROY, WM_QUIT, WM_CLOSE:
    postQuitMessage 0
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = newWideCString "More windows"
sampleWindow name, name, wndProc
