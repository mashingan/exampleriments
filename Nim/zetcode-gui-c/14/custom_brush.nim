import std/[winlean]
import ../basic_windows_05

#proc drawRectangle(hdc, brush: Handle, left, top, right, bottom): Handle =
  #result = selectObject(hdc, brush)
  #discard rectangle(hdc, left, top, right, bottom)

proc drawLines(hwnd, hbitmap: Handle) =
  var
    ps: PaintStruct
    hdc = hwnd.beginPaint ps.addr
    custbrush = createPatternBrush(hbitmap)
    holdbrush = selectObject(hdc, custbrush)
  discard selectObject(hdc, getStockObject(NULL_PEN))
  discard rectangle(hdc, 20, 20, 250, 160)
  discard selectObject(hdc, holdbrush)
  discard deleteObject(custbrush)
  discard selectObject(hdc, getStockObject(BLACK_PEN))
  discard hwnd.endPaint(ps.addr)

let bits = [0x111111ff'u32, 0xffffffff'u32, 0xffffffff'u32, 0xffffffff'u32, 0, 0, 0, 0]
var hbitmap: Handle
proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY: postQuitMessage 0
  of WM_CREATE:
    hbitmap = createBitmap(8, 8, 1, 1, cast[ByteAddress](bits[0].unsafeAddr))
  of WM_PAINT:
    drawLines(hwnd, hbitmap)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Custom brush"
sampleWindow name, name, wndproc
