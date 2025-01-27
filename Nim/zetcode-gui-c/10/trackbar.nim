import std/[winlean, with]
import ../basic_windows_05

var ghtrack, ghlabel: Handle

proc updateLabel =
  let pos = sendMessage(ghtrack, TBM_GETPOS, 0, 0)
  discard setWindowTextW(ghlabel, L $pos)

proc createControl(hwnd: Handle) =
  var icx: InitCommonControlsEx
  icx.size = int32 sizeof(icx)
  icx.icc = ICC_LISTVIEW_CLASSES
  initCommonControlsEx(icx.addr)

  let stcls = L"Static"
  let leftlabel = createWindowW(stcls, L"0", WS_CHILD or WS_VISIBLE,
                                0, 0, 10, 30, hwnd, 1, nil, nil)
  let rightlabel = createWindowW(stcls, L"100", WS_CHILD or WS_VISIBLE,
                                 0, 0, 30, 30, hwnd, 2, nil, nil)

  ghlabel = createWindowW(stcls, L"0", WS_CHILD or WS_VISIBLE,
                          270, 20, 30, 30, hwnd, 3, nil, nil)
  ghtrack = createWindowW(L TRACKBAR_CLASS, L"Trackbar control",
                          WS_CHILD or WS_VISIBLE or TBS_AUTOTICKS,
                          20, 20, 170, 30, hwnd, 3, nil, nil)
  discard sendMessage(ghtrack, TBM_SETRANGE, 1, makeLong(0, 100))
  discard sendMessage(ghtrack, TBM_SETPAGESIZE, 0, 10)
  discard sendMessage(ghtrack, TBM_SETTICFREQ, 10, 0)
  discard sendMessage(ghtrack, TBM_SETPOS, 0, 0)
  discard sendMessage(ghtrack, TBM_SETBUDDY, 1, cint leftlabel)
  discard sendMessage(ghtrack, TBM_SETBUDDY, 0, cint rightlabel)

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_CLOSE, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl(hwnd)
  of WM_HSCROLL:
    updateLabel()
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Trackbar"
sampleWindow name, name, wndproc
