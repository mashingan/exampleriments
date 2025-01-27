import std/[winlean, strformat]
import ../basic_windows_05

const
  dsize = 20
var
  hstatic, hmonthcal: Handle

proc createControl(hwnd: Handle) =
  hstatic = createWindowW(L WC_STATICW, L"", WS_CHILD or WS_VISIBLE,
                          80, 240, 80, 30, hwnd, 1, nil, nil)

  var icx: InitCommonControlsEx
  icx.size = int32 sizeof(icx)
  icx.icc = ICC_DATE_CLASSES
  initCommonControlsEx(icx.addr)
  hmonthcal = createWindowW(L MONTHCAL_CLASSW, L"", WS_BORDER or WS_CHILD or
                            WS_VISIBLE or MCS_NOTODAYCIRCLE,
                            20, 20, 200, 200, hwnd, 2, nil, nil)

proc getSelectedDate(hm, hstat: Handle) =
  var st: SystemTime
  zeroMem(st.addr, sizeof st)
  discard sendMessage(hm, MCM_GETCURSEL, 0, cast[cint](st.addr))
  let buf = L &"{st.wYear:4}-{st.wMonth:02}-{st.wDay:02}"
  discard setWindowTextW(hstat, buf)

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_CLOSE, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl(hwnd)
  of WM_NOTIFY:
    let nmhdr = (cast[ptr NmHdr](lparam))[]
    if nmhdr.code == MCN_SELECT:
      getSelectedDate(hmonthcal, hstatic)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Month calendar"
sampleWindow name, name, wndproc
