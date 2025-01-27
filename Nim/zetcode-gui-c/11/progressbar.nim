import std/[winlean]
import ../basic_windows_05

const
  idButton = 1
  idTimer = 5

var
  hprogbar, hbtn: Handle
  i = 0

proc createControl(hwnd: Handle) =
  var icx = InitCommonControlsEx(
    size: int32 sizeof(InitCommonControlsEx),
    icc: ICC_PROGRESS_CLASS,
  )
  initCommonControlsEx(icx.addr)
  hprogbar = createWindowEx(0, PROGRESS_CLASS, "", WS_CHILD or WS_VISIBLE or PBS_SMOOTH,
                            30, 20, 190, 25, hwnd, 0, nil, nil)
  hbtn = createWindowW(L"Button", L"Start", WS_CHILD or WS_VISIBLE,
                       85, 90, 85, 25, hwnd, 1, nil, nil)
  discard sendMessage(hprogbar, PBM_SETRANGE, 0, makeLong(0, 150))
  discard sendMessage(hprogbar, PBM_SETSTEP, 1, 0)

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_CLOSE, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl(hwnd)
  of WM_TIMER:
    discard sendMessage(hprogbar, PBM_STEPIT, 0, 0)
    inc i
    if i == 150:
      discard killTimer(hwnd, idTimer)
      discard sendMessage(hbtn, WM_SETTEXT, 0, cast[cint](cstring "Start"))
      i = 0
  of WM_COMMAND:
    if i == 0:
      i = 1
      discard sendMessage(hprogbar, PBM_SETPOS, 0, 0)
      discard setTimer(hwnd, idTimer, 5, nil)
      discard sendMessage(hbtn, WM_SETTEXT, 0, cast[cint](cstring "In progress"))

  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Combo box"
sampleWindow name, name, wndproc
