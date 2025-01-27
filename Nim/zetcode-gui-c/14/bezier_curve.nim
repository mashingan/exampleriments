import std/[winlean, sequtils]
import ../basic_windows_05

let points = [(20, 20), (320, 200), (330, 110), (450, 40)].mapIt(
  Point(x: clong it[0], y: clong it[1])
)
proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY: postQuitMessage 0
  of WM_PAINT:
    var ps: PaintStruct
    let hdc = hwnd.beginPaint ps.addr
    discard polyBezier(hdc, points[0].unsafeAddr, points.len)
    discard hwnd.endPaint(addr ps)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Bezier curve"
sampleWindow name, name, wndproc
