import std/[winlean, sequtils]
import ../basic_windows_05


let pg = [(30, 145), (85, 165), (105, 110), (65, 125), (30, 105)].mapIt(
  Point(x: clong it[0], y: clong it[1])
)

proc drawLines(hwnd: Handle) =
  var
    ps: PaintStruct
    hdc = hwnd.beginPaint ps.addr
  discard hdc.ellipse(30, 30, 120, 90)
  discard hdc.roundRect(150, 30, 240, 90, 15, 20)
  discard hdc.chord(270, 30, 360, 90, 270, 45, 360, 45)
  discard hdc.polygon(pg[0].unsafeAddr, pg.len)
  discard hdc.rectangle(150, 110, 230, 160)
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

let name = L"Rectangle"
sampleWindow name, name, wndproc
