import std/[winlean, sequtils]
import ../basic_windows_05

let pg = [(10, 85), (85,75), (110,10), (135,75), (210,85), (160,125),
          (170,190), (110,150), (50, 190), (60,125),(10,85)].mapIt(
            Point(x: clong it[0], y: clong it[1]))

proc drawLines(hwnd: Handle) =
  var
    ps: PaintStruct
    hdc = hwnd.beginPaint ps.addr
  discard hdc.polyline(pg[0].unsafeAddr, pg.len)
  discard hwnd.endPaint ps.addr

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY: postQuitMessage 0
  of WM_PAINT:
    drawLines(hwnd)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Star"
sampleWindow name, name, wndproc
