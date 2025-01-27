import std/[winlean, with, strformat]
import ../basic_windows_05

const
  idUpDown = 1
  idEdit = 2
  idStatic = 3
  udMaxPos = 30
  udMinPos = 0

var ghupdown, ghedit, ghstatic: Handle

proc createControl(hwnd: Handle) =
  var icx: InitCommonControlsEx
  icx.size = int32 sizeof(icx)
  icx.icc = ICC_UPDOWN_CLASS
  initCommonControlsEx(icx.addr)

  let stcls = L"Static"
  ghupdown = createWindowW(L UPDOWN_CLASS, L"",WS_CHILD or WS_VISIBLE or
                           UDS_SETBUDDYINT or UDS_ALIGNRIGHT,
                           0, 0, 0, 0, hwnd, idUpDown, nil, nil)
  ghedit = createWindowExW(WS_EX_CLIENTEDGE, L WC_EDIT, L"", WS_CHILD or
                           WS_VISIBLE or ES_RIGHT,
                           15, 15, 70, 25, hwnd, idEdit, nil, nil)

  ghstatic = createWindowW(stcls, L"0", WS_CHILD or WS_VISIBLE or SS_LEFT,
                           15, 60, 300, 30, hwnd, idStatic, nil, nil)
  discard sendMessage(ghupdown, UDM_SETPOS32, 0, 0)
  discard sendMessage(ghupdown, UDM_SETBUDDY, cuint ghedit, 0)
  discard sendMessage(ghupdown, UDM_SETRANGE, 1, makeLong(udMaxPos, udMinPos))

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_QUIT, WM_CLOSE, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl(hwnd)
  of WM_NOTIFY:
    let code = (cast[ptr NmHdr](lparam))[].code
    #let mud = (cast[ptr NmUpDown](lparam))[]
    #let code = mud.hdr.code
    if code == UDN_DELTAPOS:
      #discard
      let mud = (cast[ptr NmUpdown](lparam))[]
      var value = mud.iPos + mud.iDelta
      if value < udMinPos:
        value = udMinPos
      elif value > udMaxPos:
        value = udMaxPos

      discard setWindowTextW(ghstatic, L $value)
    else:
      let msg = &"failed to fetch value with code {code} " &
        &"expected code {UDN_DELTAPOS}"
      discard setWindowText(ghstatic, msg)

  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"Updown control"
sampleWindow name, name, wndproc
