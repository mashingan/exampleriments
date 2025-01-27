import std/[winlean]
import ../basic_windows_05

const
  idBeep = 1
  idQuit = 2


proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    discard createWindowW(L"Button", L"Beep", WS_VISIBLE or WS_CHILD,
                          20, 20, 80, 25, hwnd, idBeep, nil, nil)
    discard createWindowW(L"Button", L"Quit", WS_VISIBLE or WS_CHILD,
                          120, 20, 80, 25, hwnd, idQuit, nil, nil)
  of WM_COMMAND:
    case loword(wparam)
    of idBeep:
      discard messageBeep(MB_OK)
    of idQuit:
      postQuitMessage 0
    else: discard
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Buttons"
sampleWindow name, name, wndproc
