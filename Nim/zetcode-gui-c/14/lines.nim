import std/[winlean]
import ../basic_windows_05


proc drawLines(hwnd: Handle) =
  var
    ps: PaintStruct
    hdc = hwnd.beginPaint ps.addr

  discard hdc.moveToEx(50, 50, nil)
  discard hdc.lineTo(250, 50)

  var
    hwhitepen = getStockObject(WHITE_PEN)
    holdpen = hdc.selectObject hwhitepen
  discard hdc.moveToEx(50, 100, nil)
  discard hdc.lineTo(250, 100)

  discard deleteObject(hwhitepen)
  discard selectObject(hdc, holdpen)
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

let name = L"Lines"
sampleWindow name, name, wndproc
