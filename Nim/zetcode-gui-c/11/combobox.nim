import std/[winlean]
import ../basic_windows_05

var
  hstatic, hcombo: Handle
  ghinst: Hinstance
let items = @[cstring "FreeBSDありがとう", "OpenBSD異世界", "NetBSD魔法",
"Solaris太陽", "Arch悪魔"]

proc createControl(hwnd: Handle, inst: Hinstance) =
  hcombo = createWindowW(L"Combobox", L"", WS_CHILD or WS_VISIBLE or CBS_DROPDOWN,
                         10, 10, 120, 110, hwnd, 0, inst, nil)
  discard createWindowW(L"Button", L"Drop down", WS_CHILD or WS_VISIBLE,
                        150, 10, 90, 25, hwnd, 1, inst, nil)
  hstatic = createWindowW(L"Static", L"", WS_CHILD or WS_VISIBLE,
                         150, 80, 90, 50, hwnd, 0, inst, nil)
  for item in items:
    discard sendMessage(hcombo, CB_ADDSTRING, 0, cast[cint](item))

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_CLOSE, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl(hwnd, ghinst)
  of WM_COMMAND:
    case hiword(wparam)
    of BN_CLICKED:
      discard sendMessage(hcombo, CB_SHOWDROPDOWN, 1, 0)
    of CBN_SELCHANGE:
      let sel = sendMessage(hcombo, CB_GETCURSEL, 0, 0)
      discard setWindowText(hstatic, items[sel])
    else:
      discard
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

ghinst = getModuleHandle("")
let name = L"Combo box"
sampleWindow name, name, wndproc
