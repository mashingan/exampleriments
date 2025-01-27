import std/[winlean, strformat]
import ../basic_windows_05

type
  Friend = object
    name: string
    job: string
    age: Natural

const
  idList = 1
  idStatic = 2

var hlist, hstatic: Handle
let friends = @[
  Friend(name: "Lucy", job: "waitress", age: 18),
  Friend(name: "Thomas", job: "programmer", age: 25),
  Friend(name: "George", job: "police officer", age: 26),
  Friend(name: "Michael", job: "producer", age: 38),
  Friend(name: "Jane", job: "steward", age: 28),
]

proc createControl(hwnd: Handle) =
  hlist = createWindowW(L WC_LISTBOXA, L"", WS_CHILD or WS_VISIBLE or LBS_NOTIFY,
                        10, 10, 150, 120, hwnd, idList, nil, nil)
  hstatic = createWindowW(L WC_STATICW, L"", WS_CHILD or WS_VISIBLE,
                          200, 10, 120, 45, hwnd, idStatic, nil, nil)
  for friend in friends:
    discard sendMessage(hlist, LB_ADDSTRING, 0, cast[cint](cstring friend.name))

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl hwnd
  of WM_COMMAND:
    if loword(wparam) == idList and hiword(wparam) == LBN_SELCHANGE:
      let
        sel = sendMessage(hlist, LB_GETCURSEL, 0, 0)
        friend = friends[sel]
        msg = &"Job: {friend.job}\nAge: {friend.age}"
      discard setWindowTextW(hstatic, L msg)
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"List box"
sampleWindow name, name, wndproc
