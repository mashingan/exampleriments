import std/[winlean, random]
import ../basic_windows_05


proc drawLines(hwnd: Handle) =
  var
    ps: PaintStruct
    r: Rect
  discard getClientRect(hwnd, addr r)
  if r.bottom == 0:
    return
  var hdc = hwnd.beginPaint ps.addr
  for i in 0 ..< 1000:
    var x = rand r.right
    var y = rand r.bottom
    discard hdc.setPixel(x, y, rgb(255, 0, 0))
  discard hwnd.endPaint ps.addr

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY: postQuitMessage 0
  of WM_CREATE:
    discard
  of WM_PAINT:
    drawLines(hwnd)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

randomize()
let name = L"Pixels paint"
sampleWindow name, name, wndproc
