import std/[winlean, os, strformat]
import ../basic_windows_05

proc drawRectangle(hdc, hbrush: Handle, left, top, right, bottom: int): Handle =
  result = selectObject(hdc, hbrush)
  discard rectangle(hdc, left, top, right, bottom)

proc drawLines(hwnd, hbitmap: Handle) =
  var
    ps: PaintStruct
    hdc = hwnd.beginPaint ps.addr
    hpen = createPen(PS_NULL, 1, rgb(0, 0, 0))
    holdpen = selectObject(hdc, hpen)
    brushes = [
      createHatchBrush(HS_BDIAGONAL, rgb(121, 90, 0)),
      createHatchBrush(HS_FDIAGONAL, rgb(240, 53, 19)),
      createHatchBrush(HS_CROSS, rgb(240, 210, 18)),
      createHatchBrush(HS_HORIZONTAL, rgb(9, 189, 21)),
      createHatchBrush(HS_DIAGCROSS, rgb(0, 0, 0)),
      createHatchBrush(HS_VERTICAL, rgb(0, 0, 0)),
    ]
    col = getSysColor COLOR_BTNFACE

  discard setBkColor(hdc, col)
  var holdbrush = drawRectangle(hdc, brushes[0], 30, 30, 100, 80)
  discard drawRectangle(hdc, brushes[1], 110, 30, 180, 80)
  discard drawRectangle(hdc, brushes[2], 190, 30, 260, 80)
  discard drawRectangle(hdc, brushes[3], 30, 110, 100, 160)
  discard drawRectangle(hdc, brushes[4], 110, 110, 180, 160)
  discard drawRectangle(hdc, brushes[5], 190, 110, 260, 160)

  discard selectObject(hdc, holdpen)
  discard selectObject(hdc, holdbrush)
  discard deleteObject(hpen)
  for brush in brushes:
    discard deleteObject(brush)
  discard hwnd.endPaint ps.addr

var hbitmap: Handle

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY: postQuitMessage 0
  of WM_PAINT:
    drawLines(hwnd, hbitmap)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Draw bitmap"
sampleWindow name, name, wndproc
