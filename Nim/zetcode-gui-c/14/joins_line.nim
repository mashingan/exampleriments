import std/[winlean, sequtils, with]
import ../basic_windows_05


proc drawLines(hwnd: Handle) =
  var
    ps: PaintStruct
    brush: LogBrush
    col = rgb(0, 0, 0)
    penstyle = PS_SOLID or PS_JOIN_MITER or PS_GEOMETRIC
    hdc = hwnd.beginPaint ps.addr

  with brush:
    lbStyle = BS_SOLID
    lbColor = col
    lbHatch = nil

  var
    hpen1 = extCreatePen(penstyle, 8, brush.addr, 0, nil)
    holdpen = selectObject(hdc, hpen1)
    points = [(30,30), (130, 30), (130,100), (30,100), (30,30)].mapIt(
      Point(x: clong it[0], y: clong it[1])
    )
  discard polygon(hdc, points[0].addr, points.len)

  penstyle = PS_SOLID or PS_GEOMETRIC or PS_JOIN_BEVEL
  var hpen2 = extCreatePen(penstyle, 8, addr brush, 0, nil)
  discard selectObject(hdc, hpen2)
  discard deleteObject(hpen1)
  var points2 = [(160,30), (260, 30), (260,100), (160,100), (160,30)].mapIt(
      Point(x: clong it[0], y: clong it[1])
  )
  discard hdc.moveToEx(130, 30, nil)
  discard polygon(hdc, addr points2[0], points2.len)

  penstyle = PS_SOLID or PS_GEOMETRIC or PS_JOIN_ROUND
  var hpen3 = extCreatePen(penstyle, 8, addr brush, 0, nil)
  discard selectObject(hdc, hpen3)
  discard deleteObject(hpen2)
  var points3 = [(290,30), (390, 30), (390,100), (290,100), (290,30)].mapIt(
      Point(x: clong it[0], y: clong it[1])
  )
  discard hdc.moveToEx(260, 30, nil)
  discard polygon(hdc, addr points3[0], points3.len)

  discard selectObject(hdc, holdpen)
  discard deleteObject(hpen3)

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

let name = L"Line joins"
sampleWindow name, name, wndproc
