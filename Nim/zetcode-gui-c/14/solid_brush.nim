import std/[winlean, sequtils]
import ../basic_windows_05

proc drawRectangle(hdc, hbrush: Handle, left, top, right, bottom: int): Handle =
  result = selectObject(hdc, hbrush)
  discard rectangle(hdc, left, top, right, bottom)

proc drawLines(hwnd: Handle) =
  var
    ps: PaintStruct
    hdc = hwnd.beginPaint ps.addr
    hpen = createPen(PS_NULL, 1, rgb(0, 0, 0))
    holdpen = hdc.selectObject hpen
    brushes = [
      (121, 90, 0),
      (240, 63, 19),
      (240, 210, 18),
      (9, 189, 21),
    ].mapIt(createSolidBrush rgb(byte it[0], byte it[1], byte it[2]))
    holdbrush = hdc.drawRectangle(brushes[0], 30, 30, 100, 100)
  discard hdc.drawRectangle(brushes[1], 110, 30, 180, 100)
  discard hdc.drawRectangle(brushes[2], 30, 110, 100, 180)
  discard hdc.drawRectangle(brushes[3], 110, 110, 180, 180)

  discard hdc.selectObject holdpen
  discard hdc.selectObject holdbrush
  discard deleteObject hpen
  for brush in brushes:
    discard deleteObject brush

  discard hwnd.endPaint ps.addr

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY: postQuitMessage 0
  of WM_PAINT:
    drawLines(hwnd)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Solid brush"
sampleWindow name, name, wndproc
