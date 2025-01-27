import std/[winlean, sequtils]
import ../basic_windows_05

proc straigthLine(hdc, hpen: Handle, y: int): Handle =
  result = hdc.selectObject hpen
  discard hdc.moveToEx(50, y, nil)
  discard hdc.lineTo(300, y)

proc drawLines(hwnd: Handle) =
  var
    ps: PaintStruct
    hdc = hwnd.beginPaint ps.addr
    hpens = [
      (PS_SOLID, rgb(0, 0, 0)),
      (PS_DASH, rgb(0, 0, 0)),
      (PS_DOT, rgb(0, 0, 0)),
      (PS_DASHDOT, rgb(0, 0, 0)),
      (PS_DASHDOTDOT, rgb(0, 0, 0)),
    ].mapIt(createPen(it[0], 1, it[1]))
  let start = 30
  var holdpen = hdc.straigthLine(hpens[0], start)
  let step = 20
  for i in 1 .. hpens.high:
    discard hdc.straigthLine(hpens[i], start + i * step)
  discard hdc.selectObject holdpen
  for hpen in hpens:
    discard deleteObject(hpen)
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

let name = L"Pen styles"
sampleWindow name, name, wndproc
