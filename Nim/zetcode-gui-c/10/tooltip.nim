import std/[winlean, with, sugar]
import ../basic_windows_05

proc createControl(hwnd: Handle) =
  var icx: InitCommonControlsEx
  icx.size = int32 sizeof(icx)
  icx.icc = ICC_WIN95_CLASSES
  initCommonControlsEx(icx.addr)

  var ti: ToolInfoA
  static:
    dump WS_POPUP
    dump TTS_NOPREFIX
    dump TTS_ALWAYSTIP
    dump WS_POPUP or TTS_NOPREFIX or TTS_ALWAYSTIP
  var htooltip = createWindowExW(WS_EX_TOPMOST, L TOOLTIPS_CLASSW, L"",
                                 WS_POPUP or TTS_NOPREFIX or TTS_ALWAYSTIP,
                                 0, 0, 0, 0, hwnd, 0, nil, nil)
  setWindowPos(htooltip, HWND_TOPMOST, 0, 0, 0, 0,
               SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE)
  var rect: Rect
  discard getClientRect(hwnd, rect.addr)
  with ti:
    size = uint32 sizeof(ti)
    flags = TTF_SUBCLASS
    hwnd = hwnd
    instance = nil
    id = 0
    text = "A main window"
  copyMem(ti.rect.addr, rect.addr, sizeof(rect))
  discard sendMessage(htooltip, TTM_ADDTOOLA, 0, cast[cint](ti.addr))

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_CLOSE, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl(hwnd)
  of WM_NOTIFY:
    discard
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Tooltip"
sampleWindow name, name, wndproc
