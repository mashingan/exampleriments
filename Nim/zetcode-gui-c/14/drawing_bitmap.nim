import std/[winlean, os, strformat]
import ../basic_windows_05

const fname = currentSourcePath.parentDir.parentDir() / "logo-drawing-for-tut14.bmp"

proc drawLines(hwnd, hbitmap: Handle) =
  var
    ps: PaintStruct
    bitmap: Bitmap
    hdc = hwnd.beginPaint ps.addr
    hdcmem = createCompatibleDc hdc
    oldbitmap = selectObject(hdcmem, hbitmap)
  discard getObject(hbitmap, sizeof(bitmap), bitmap.addr)
  discard bitBlt(hdc, 5, 5, bitmap.bmWidth, bitmap.bmHeight, hdcmem, 0, 0, SRCCOPY)
  discard selectObject(hdcmem, oldbitmap)
  discard deleteDC(hdcmem)
  discard hwnd.endPaint ps.addr

var hbitmap: Handle

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY: postQuitMessage 0
  of WM_CREATE:
    hbitmap = loadImageW(nil, L fname, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE)
    if hbitmap == 0:
      let msg = &"Failed to load image path {fname}"
      discard messageBoxW(hwnd, L msg, L"Error", MB_OK or MB_ICONERROR)
  of WM_PAINT:
    drawLines(hwnd, hbitmap)
  else: discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Draw bitmap"
sampleWindow name, name, wndproc
