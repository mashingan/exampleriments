import std/[winlean]
import ../basic_windows_05

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    discard createWindowW(L"Button", L"Show title",
                          WS_VISIBLE or WS_CHILD or BS_CHECKBOX,
                          20, 20, 80, 25, hwnd, 1, nil, nil)
    discard checkDlgButton(hwnd, 1, BST_CHECKED)
  of WM_COMMAND:
    let checked = isDlgButtonChecked(hwnd, 1)
    if checked:
      discard checkDlgButton(hwnd, 1, BST_UNCHECKED)
      discard setWindowTextW(hwnd, L"")
    else:
      discard checkDlgButton(hwnd, 1, BST_CHECKED)
      discard setWindowTextW(hwnd, L"Check box")
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Check box"
sampleWindow name, name, wndproc
