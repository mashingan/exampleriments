import std/[winlean, strformat]
import ../basic_windows_05


const
  idTabCtrl = 1
  idEdit = 2
  btnAdd = 3
  btnDel = 4
  btnClr = 5
  maxTablen = 15

var htab, hedit: Handle

proc createControl(hwnd: Handle) =
  var icx = InitCommonControlsEx(
    size: int32 sizeof(InitCommonControlsEx),
    icc: ICC_TAB_CLASSES
  )
  initCommonControlsEx(icx.addr)

  htab = createWindowW(L WC_TABCONTROLA, L"", WS_CHILD or WS_VISIBLE,
                       0, 0, 200, 150, hwnd, idTabCtrl, nil, nil)
  hedit = createWindowW(L WC_EDIT, L"", WS_CHILD or WS_VISIBLE or WS_BORDER,
                        250, 20, 100, 25, hwnd, idEdit, nil, nil)
  let bcl = L"Button"
  discard sendMessage(hedit, EM_SETLIMITTEXT, maxTablen, 0)
  for (label, x, y, cx, cy, menuId) in @[
    (L"Add", 250, 50, 100, 25, btnAdd),
    (L"Delete", 250, 80, 100, 25, btnDel),
    (L"Clear", 250, 110, 100, 25, btnClr),
  ]:
    discard createWindowW(bcl, label, WS_CHILD or WS_VISIBLE or BS_PUSHBUTTON,
                          x, y, cx, cy, hwnd, menuId, nil, nil)

proc wndproc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
  case msg
  of WM_CLOSE, WM_QUIT, WM_DESTROY:
    postQuitMessage 0
  of WM_CREATE:
    createControl hwnd
  of WM_COMMAND:
    case loword(wparam)
    of btnAdd:
      var text = cstring newString(250)
      discard getWindowText(hedit, text, 250)
      if text.len > 0:
        var tie = TcItem(
          mask: TCIF_TEXT,
          text: text,
        )
        let count = sendMessage(htab, TCM_GETITEMCOUNT, 0, 0)
        discard sendMessage(htab, TCM_INSERTITEMA, cast[cuint](count), cast[cint](tie.addr))
    of btnDel:
      let id = sendMessage(htab, TCM_GETCURSEL, 0, 0)
      if id != -1:
        discard sendMessage(htab, TCM_DELETEITEM, cuint id, 0)
    of btnClr:
      discard sendMessage(htab, TCM_DELETEALLITEMS, 0, 0)
    else: discard
  else:
    discard
  defWindowProcW(hwnd, msg, wparam, lparam)

let name = L"List box"
sampleWindow name, name, wndproc
