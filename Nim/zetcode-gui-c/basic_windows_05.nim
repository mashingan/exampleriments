import ./winconst.nim
import std/winlean

{.pragma: winh, header: "<windows.h>".}
{.pragma: comh, header: "<commctrl.h>".}
{.pragma: dlgh, header: "<commdlg.h>".}
{.pragma: gdih, header: "<wingdi.h>".}
{.pragma: gdip, header: "<gdiplus.h>".}

converter toBool*(w: WINBOOL): bool =
  w == 1

template `as`(a, b: untyped): untyped = cast[`b`](`a`)

export winconst

type
  Hinstance* {.importc: "HINSTANCE", winh.} = pointer
  Message* {.importc: "MSG", winh.} = object
    wparam* {.importc: "wParam".}: ptr uint
  WndCb* = proc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.}
  TimerCb* = proc(hwnd: Handle; msg, event: uint, systime: int) {.cdecl.}
  WndClassW* {.importc: "WNDCLASSW", winh.} = object
    style* {.importc.}: int
    classExtra* {.importc: "cbClsExtra".}: int
    wndExtra* {.importc: "cbWndExtra".}: int
    className* {.importc: "lpszClassName".}: WideCString
    instance* {.importc: "hInstance".}: Hinstance
    background* {.importc: "hbrBackground".}: Handle
    menuName* {.importc: "lpszMenuName".}: WideCString
    wndProc* {.importc: "lpfnWndProc".}: WndCb
    cursor* {.importc: "hCursor".}: Handle
    icon* {.importc: "hIcon".}: Handle
  WndClassExW* {.importc: "WNDCLASSEXW", winh.} = object
    size* {.importc: "cbSize".}: uint32
    style* {.importc.}: uint32
    wndProc* {.importc: "lpfnWndProc".}: WndCb
    classExtra* {.importc: "cbClsExtra".}: cint
    wndExtra* {.importc: "cbWndExtra".}: cint
    instance* {.importc: "hInstance".}: Hinstance
    icon* {.importc: "hIcon".}: Handle
    cursor* {.importc: "hCursor".}: Handle
    background* {.importc: "hbrBackground".}: Handle
    menuName* {.importc: "lpszMenuName".}: WideCString
    className* {.importc: "lpszClassName".}: WideCString
    iconSm* {.importc: "hIconSm".}: Handle

  Rect* {.importc: "RECT", winh.} = object
    left* {.importc.}: clong
    top* {.importc.}: clong
    right* {.importc.}: clong
    bottom* {.importc.}: clong
  FlashwInfo* {.importc: "FLASHWINFO", winh.} = object
    size* {.importc: "cbSize"}: uint
    hwnd* {.importc.}: Handle
    flags* {.importc: "dwFlags".}: int32
    count* {.importc: "uCount".}: uint
    timeout* {.importc: "dwTimeout".}: int32
  InitCommonControlsEx* {.importc: "INITCOMMONCONTROLSEX", comh.} = object
    size* {.importc: "dwSize".}: int32
    icc* {.importc: "dwICC".}: int32

  Point* {.importc: "POINT", winh.} = object
    x*: clong
    y*: clong

  ColorRef* {.importc: "COLORREF", winh.} = int32
  ChooseColorW* {.importc: "CHOOSECOLORW", dlgh.} = object
    size* {.importc: "lStructSize".}: int32
    owner* {.importc: "hwndOwner".}: Handle
    instance* {.importc: "hInstance".}: Handle
    rgbResult* {.importc.}: ColorRef
    custColors* {.importc: "lpCustColors".}: ptr ColorRef
    flags* {.importc: "Flags".}: int32
    custData* {.importc: "lCustData".}: cuint
    hook* {.importc: "lpfnHook".}: WndCb
    templateName* {.importc: "lpTemplateName".}: WideCString
  PaintStruct* {.importc: "PAINTSTRUCT", winh.} = object
    hdc*: Handle
    erase* {.importc: "fErase".}: bool
    rcPaint*: Rect
    restore* {.importc: "fRestore".}: bool
    incUpdate* {.importc: "fIncUpdate".}: bool
    rgbReserved*: array[32, byte]
  OpenFileNameW* {.importc: "OPENFILENAMEW", dlgh.} = object
    size* {.importc: "lStructSize".}: int32
    owner* {.importc: "hwndOwner".}: Handle
    instance* {.importc: "hInstance".}: Hinstance
    filter* {.importc: "lpstrFilter".}: WideCString
    customFilter* {.importc: "lpstrCustomFilter".}: WideCString
    maxCustomFilter* {.importc: "nMaxCustFilter".}: int32
    filterIndex* {.importc: "nFilterIndex".}: int32
    file* {.importc: "lpstrFile".}: WideCString
    maxFile* {.importc: "nMaxFile".}: int32
    fileTitle* {.importc: "lpstrFileTitle".}: WideCString
    maxFileTitle* {.importc: "nMaxFileTitle".}: int32
    initialDir* {.importc: "lpstrInitialDir".}: WideCString
    title* {.importc: "lpstrTitle".}: WideCString
    flags* {.importc: "Flags".}: int32
    fileOffset* {.importc: "nFileOffset".}: int16
    fileExt* {.importc: "nFileExtension".}: int16
    defaultExt* {.importc: "lpstrDefExt".}: WideCString
    custData*{.importc: "lCustData".}: ptr int32
    hook* {.importc: "lpfnHook".}: WndCb
    templateName* {.importc: "lpTemplateName".}: WideCString
    preserved* {.importc: "pvReserved".}: pointer
    nreserved* {.importc: "dwReserved".}: int32
    flagsEx* {.importc: "FlagsEx".}: int32
  GpStatus* {.importc, gdip.} = enum
    Ok = 0,
    GenericError = 1,
    InvalidParameter = 2,
    OutOfMemory = 3,
    ObjectBusy = 4,
    InsufficientBuffer = 5,
    NotImplemented = 6,
    Win32Error = 7,
    WrongState = 8,
    Aborted = 9,
    FileNotFound = 10,
    ValueOverflow = 11,
    AccessDenied = 12,
    UnknownImageFormat = 13,
    FontFamilyNotFound = 14,
    FontStyleNotFound = 15,
    NotTrueTypeFont = 16,
    UnsupportedGdiplusVersion = 17,
    GdiplusNotInitialized = 18,
    PropertyNotFound = 19,
    PropertyNotSupported = 20,
    ProfileNotFound = 21
  SystemTime* {.importc: "SYSTEMTIME", winh.} = object
    wYear*: uint16
    wMonth*: uint16
    wDayOfWeek*: uint16
    wDay*: uint16
    wHour*: uint16
    wMinute*: uint16
    wSecond*: uint16
    wMilliseconds*: uint16
  NmHdr* {.importc: "NMHDR", winh.} = object
    hwndFrom*: Handle
    idFrom*: uint
    code*: uint
  ToolInfoA* {.importc: "TOOLINFOA", comh.} = object
    size* {.importc: "cbSize".}: uint32
    flags* {.importc: "uFlags".}: uint32
    hwnd*: Handle
    id* {.importc: "uId".}: uint
    rect*: Rect
    instance* {.importc: "hinst".}: Hinstance
    text* {.importc: "lpszText".}: cstring
    lparam* {.importc: "lParam".}: cint
    lpReserved*: pointer
  ToolInfoW* {.importc: "TOOLINFOW", comh.} = object
    size* {.importc: "cbSize".}: uint32
    flags* {.importc: "uFlags".}: uint32
    hwnd*: Handle
    id* {.importc: "uId".}: uint
    rect*: Rect
    instance* {.importc: "hinst".}: Hinstance
    text* {.importc: "lpszText".}: WideCString
    lparam* {.importc: "lParam".}: cint
    lpReserved*: pointer
  NmUpdown* {.importc: "NMUPDOWN", comh.} = object
    hdr*: NmHdr
    iPos*: int
    iDelta*: int
  PbRange* {.importc: "PBRANGE", comh.} = object
    iLow*: int
    iHigh*: int
  TcItemW* {.importc: "TCITEMW", comh.} = object
    mask*: uint32
    dwState*: int32
    dwStateMask*: int32
    text* {.importc: "pszText".}: WideCString
    cchTextMax*: int
    iImage*: int
    lParam*: cint
  TcItem* {.importc: "TCITEMA", comh.} = object
    mask*: uint32
    dwState*: int32
    dwStateMask*: int32
    text* {.importc: "pszText".}: cstring
    cchTextMax*: int
    iImage*: int
    lParam*: cint
  Bitmap* {.importc: "BITMAP", gdih.} = object
    bmType*: clong
    bmWidth*: clong
    bmHeight*: clong
    bmWidthBytes*: clong
    bmPlanes*: int16
    bmBitsPixel*: int16
    bmBits*: pointer
  LogBrush* {.importc: "LOGBRUSH", gdih.} = object
    lbStyle*: uint32
    lbColor*: ColorRef
    lbHatch*: ptr uint

let
  idcArrow* {.importc: "IDC_ARROW", winh.}: cstring
  idiApplication* {.importc: "IDI_APPLICATION", winh.}: cstring
  csHRedraw* {.importc: "CS_HREDRAW", winh.}: int
  csVRedraw* {.importc: "CS_VREDRAW", winh.}: int
  color3dFace* {.importc: "COLOR_3DFACE", winh.}: int
  colorBtnFace* {.importc: "COLOR_BTNFACE", winh.}: int
  wsOverlappedWindow* {.importc: "WS_OVERLAPPEDWINDOW", winh.}: int
  wsVisible* {.importc: "WS_VISIBLE", winh.}: int
  statusClassNameW* {.importc: "STATUSCLASSNAMEW", comh.}: WideCString
  statusClassName* {.importc: "STATUSCLASSNAME", comh.}: cstring

proc postQuitMessage*(code: int) {.importc: "PostQuitMessage", winh.}
proc defWindowProcW*(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int
                   {.importc: "DefWindowProcW", winh.}
proc translateMessage*(pmsg: ptr Message) {.importc: "TranslateMessage", winh.}
proc dispatchMessage*(pmsg: ptr Message) {.importc: "DispatchMessage", winh.}
proc registerClassW*(w: ptr WndClassW) {.importc: "RegisterClassW", winh.}
proc registerClassExW*(w: ptr WndClassExW) {.importc: "RegisterClassExW", winh.}
proc showWindow*(h: Handle, cmdshow: cint) {.importc: "ShowWindow", winh.}
proc updateWindow*(h: Handle) {.importc: "UpdateWindow", winh.}
proc getMessage*(pmsg: ptr Message, hwnd: Handle, firstmsg, lastmsg: uint): WINBOOL
                {.importc: "GetMessage", winh.}
proc getSysColor*(index: int): int32 {.importc: "GetSysColor", winh.}
proc getSysColorBrush*(index: int): Handle {.importc: "GetSysColorBrush", winh.}
proc loadCursor*(instance: Handle, rsc: cstring): Handle {.importc: "LoadCursor", winh.}
proc loadIcon*(instance: Handle, rsc: cstring): Handle {.importc: "LoadIcon", winh.}
proc createWindowW*(classname, windowname: WideCString, style: int, x, y, width, height: int,
                    parent, menu: Handle, instance: Hinstance, param: pointer): Handle
                   {.importc: "CreateWindowW", winh.}
proc createWindowExW*(exStyle: int, classname, windowname: WideCString, style: int,
                      x, y, width, height: int, parent, menu: Handle,
                      instance: Hinstance, param: pointer): Handle
                     {.importc: "CreateWindowExW", winh.}
proc createWindowEx*(exStyle: int, classname, windowname: cstring, style: int,
                     x, y, width, height: int, parent, menu: Handle,
                      instance: Hinstance, param: pointer): Handle
                    {.importc: "CreateWindowEx", winh.}
proc getModuleHandle*(modulename: cstring): Hinstance
                     {.importc: "GetModuleHandle", winh.}
proc getSystemMetrics*(index: int): int {.importc: "GetSystemMetrics", winh.}
proc getWindowRect*(hwnd: Handle, rc: ptr Rect) {.importc: "GetWindowRect", winh.}
proc setWindowPos*(hwnd, insertAfter: Handle, x, y, cx, cy: int, flags: uint)
                  {.importc: "SetWindowPos", winh.}
proc setWindowTextW*(hwnd: Handle; text: WideCString): WINBOOL
                    {.importc: "SetWindowTextW", winh.}
proc setWindowText*(hwnd: Handle; text: cstring): WINBOOL
                   {.importc: "SetWindowText", winh.}
proc messageBoxW*(hwnd: Handle, text, label: WideCString, boxtype: uint): int32
                 {.importc: "MessageBoxW", winh.}
proc sendMessage*(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): int
                 {.importc: "SendMessage", winh.}
proc sendMessageW*(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): int
                  {.importc: "SendMessageW", winh.}
proc flashWindowEx*(pfwi: ptr FlashwInfo){.importc: "FlashWindowEx", winh.}
proc registerHotKey*(hwnd: Handle, id: int, modifiers: uint, vk: int)
                    {.importc: "RegisterHotKey", winh.}
proc unregisterHotKey*(hwnd: Handle, id: int)
                      {.importc: "UnregisterHotKey", winh.}

proc messageBeep*(idtype: uint): WINBOOL {.importc: "MessageBeep", winh.}
proc createSolidBrush*(color: int32): Handle {.importc: "CreateSolidBrush", gdih.}
{.passL: "-luser32".}

proc createMenu*: Handle {.importc: "CreateMenu", winh.}
proc createPopupMenu*: Handle {.importc: "CreatePopupMenu", winh.}
proc appendMenuW*(menu: Handle; flags, newitemid: uint, label: WideCString): WINBOOL
                {.importc: "AppendMenuW", winh.}
proc setMenu*(hwnd: Handle, menu: Handle) {.importc: "SetMenu", winh.}
proc checkMenuItem*(menu: Handle, checkitem, state: uint)
                   {.importc: "CheckMenuItem", winh.}
proc getMenuState*(menu: Handle, id, flags: uint): uint
                  {.importc: "GetMenuState", winh.}
proc clientToScreen*(hwnd: Handle, ppoint: ptr Point)
                    {.importc: "ClientToScreen", winh.}
proc trackPopupMenu*(menu: Handle, flags: uint, x, y, reserved: int, hwnd: Handle,
                     prect: ptr Rect) {.importc: "TrackPopupMenu", winh.}
proc destroyMenu*(menu: Handle) {.importc: "DestroyMenu", winh.}
proc destroyWindow*(hwnd: Handle) {.importc: "DestroyWindow", winh.}
proc checkMenuRadioItem*(menu: Handle, first, last, check, flags: uint): WINBOOL
                        {.importc: "CheckMenuRadioItem", winh.}

proc initCommonControls* {.importc: "InitCommonControls", comh.}
proc initCommonControlsEx*(icx: ptr InitCommonControlsEx)
                          {.importc: "InitCommonControlsEx", comh.}
{.passL: "-lcomctl32".}

proc loword*(a: uint): cint {.importc: "LOWORD", winh.}
proc hiword*(a: uint): cint {.importc: "HIWORD", winh.}
proc makeLong*(a, b: SomeInteger): cint {.importc: "MAKELONG", winh.}

proc getStockObject*(id: int): Handle {.importc: "GetStockObject", gdih.}
proc chooseColor*(pcc: ptr ChooseColorW) {.importc: "ChooseColor", dlgh.}

{.passL: "-lcomdlg32".}
{.passL: "-lgdi32".}
#{.passL: "-lgdiplus".}

proc getClientRect*(hwnd: Handle, prect: ptr Rect): WINBOOL
                   {.importc: "GetClientRect", winh.}
proc beginPaint*(hwnd: Handle, ppaint: ptr PaintStruct): Handle
                {.importc: "BeginPaint", winh.}
proc endPaint*(hwnd: Handle, ppaint: ptr PaintStruct): WINBOOL
              {.importc: "EndPaint", winh.}

proc setBkColor*(hdc: Handle, color: ColorRef): ColorRef
                {.importc: "SetBkColor", gdih.}
proc setDCBrushColor*(hdc: Handle, color: ColorRef): ColorRef
                     {.importc: "SetDCBrushColor", gdih.}
proc setDCPenColor*(hdc: Handle, color: ColorRef): ColorRef
                   {.importc: "SetDCPenColor", gdih.}
proc textOutW*(hdc: Handle, x, y: int, text: WideCString, length: int): WINBOOL
              {.importc: "TextOutW", gdih.}
proc extTextOut*(hdc: Handle, x, y: int; options: uint, prect: ptr Rect,
                 text: WideCString, c: uint, dx: ptr int)
                {.importc: "ExtTextOut", gdih.}

proc invalidateRect*(hwnd: Handle, prect: ptr Rect, erase: bool): WinBool
                    {.importc: "InvalidateRect", winh.}
proc validateRect*(hwnd: Handle, prect: ptr Rect): WinBool
                  {.importc: "ValidateRect", winh.}
proc getOpenFileNameW*(pofn: ptr OpenFileNameW): WINBOOL
                      {.importc: "GetOpenFileNameW", dlgh.}
proc createFileW*(filename: WideCString, access, sharemode: int,
                  securityAttr: ptr SECURITY_ATTRIBUTES, createDisposition: int,
                  flagsAttr: int, templateFile: Handle): Handle
                 {.importc: "CreateFileW", winh.}
#proc getFileSize*(file: Handle, filesizeHigh: ptr int): int
#                 {.importc: "GetFileSize", winh.}
proc getProcessHeap*: Handle {.importc: "GetProcessHeap", winh.}
proc heapAlloc*(heap: Handle, flags, bytes: int): pointer
               {.importc: "HeapAlloc", winh.}
proc heapFree*(heap: Handle, flags: int, mem: pointer): WINBOOL
              {.importc: "HeapFree", winh.}
proc readFile*(file: Handle, buffer: pointer, length: int, readed: ptr int,
               poverlapped: ptr OVERLAPPED): WINBOOL
              {.importc: "ReadFile", winh.}
#proc closeHandle*(handle: Handle) {.importc: "CloseHandle", winh.}
proc checkDlgButton*(dlg: Handle, idbutton: int, check: uint): WINBOOL
                    {.importc: "CheckDlgButton", winh.}
proc checkRadioButton*(dlg: Handle, first, last, checkbutton: int): WINBOOL
                      {.importc: "CheckRadioButton", winh.}
proc isDlgButtonChecked*(dlg: Handle, idButton: int): WINBOOL
                        {.importc: "IsDlgButtonChecked", winh.}
proc getWindowTextLengthW*(hwnd: Handle): int
                          {.importc: "GetWindowTextLengthW", winh.}
proc getWindowText*(hwnd: Handle, text: cstring, length: int): WINBOOL
                   {.importc: "GetWindowTextA", winh.}
proc getWindowTextW*(hwnd: Handle, text: WideCString, length: int): WINBOOL
                    {.importc: "GetWindowTextW", winh.}

proc loadImageW*(inst: Hinstance, filename: WideCString, idtype, cx, cy, load: int):
  Handle {.importc: "LoadImageW", winh.}
#proc gdipCreateBitmapFromFile*(filename: WideCString, sink: ptr ptr Bitmap): GpStatus
                              #{.importc: "GdipCreateBitmapFromFile", gdip.}
proc deleteObject*(handle: Handle): WINBOOL {.importc: "DeleteObject", gdih.}
proc setTimer*(hwnd: Handle, idEvent, elapseMs: uint; cb: TimerCb): uint
              {.importc: "SetTimer", winh.}
proc killTimer*(hwnd: Handle, idEvent: uint): WINBOOL
               {.importc: "KillTimer", winh.}
#proc createSolidBrush*(color: ColorRef): Handle
#                      {.importc: "CreateSolidBrush", gdih.}
proc deleteDC*(hdc: HDC): WINBOOL {.importc: "DeleteDC", gdih.}
proc bitBlt*(hdc: Handle, x, y, cx, cy: int; src: Handle,
             x1, y1: int, rop: int32): WINBOOL {.importc: "BitBlt", gdih.}
proc createHatchBrush*(hatch: int, color: ColorRef): Handle
                      {.importc: "CreateHatchBrush", gdih.}
proc createPen*(style, cwidth: int, color: ColorRef): Handle
               {.importc: "CreatePen", gdih.}
proc createPatternBrush*(hbitmap: Handle): Handle
                        {.importc: "CreatePatternBrush", gdih.}
proc selectObject*(hdc, gdiobj: Handle): Handle {.importc: "SelectObject", gdih.}
proc rectangle*(hdc: Handle, left, top, right, bottom: int): WINBOOL
               {.importc: "Rectangle", gdih.}
proc rgb*(r, g, b: byte): ColorRef {.importc: "RGB", gdih.}
proc createFontW*(height, width, escape, orientation, weight: int;
                  italic, underline, strikeout: bool;
                  charset, outprecision, clipPrecision, quality, pitchFamily: int;
                  name: WideCString): Handle {.importc: "CreateFontW", gdih.}
proc moveToEx*(hdc: Handle, x, y: int, ppoint: ptr Point): WINBOOL
              {.importc: "MoveToEx", gdih.}
proc lineTo*(hdc: Handle, x, y: int): WINBOOL {.importc: "LineTo", gdih.}

  #WINUSERAPI int WINAPI DrawTextA(HDC hdc,LPCSTR lpchText,int cchText,LPRECT lprc,UINT format);
  #WINUSERAPI int WINAPI DrawTextExA(HDC hdc,LPSTR lpchText,int cchText,LPRECT lprc,UINT format,LPDRAWTEXTPARAMS lpdtp);
  #WINUSERAPI int WINAPI DrawTextExW(HDC hdc,LPWSTR lpchText,int cchText,LPRECT lprc,UINT format,LPDRAWTEXTPARAMS lpdtp);
proc drawTextW*(hdc: Handle, text: WideCString, length: int, prect: ptr Rect,
                format: uint): int {.importc: "DrawTextW", winh.}

proc setBkMode*(hdc: Handle, mode: int): int {.importc: "SetBkMode", gdih.}
proc extCreatePen*(penstyle, width: int; logbrush: ptr LogBrush,
                   cstyle: int32, pstyle: ptr int32): Handle
                  {.importc: "ExtCreatePen", gdih.}
proc polygon*(hdc: Handle, ppoint: ptr Point, cpt: int): WINBOOL
             {.importc: "Polygon", gdih.}
proc polyline*(hdc: Handle, ppoint: ptr Point, cpt: int): WINBOOL
              {.importc: "Polyline", gdih.}
proc polyBezier*(hdc: Handle, ppoint: ptr Point, cpt: int): WINBOOL
                {.importc: "PolyBezier", gdih.}
proc polyBezierTo*(hdc: Handle, ppoint: ptr Point, cpt: int): WINBOOL
                  {.importc: "PolyBezierTo", gdih.}
proc createBitmap*(width, height, planes, bitcount, bits: ByteAddress): Handle
                  {.importc: "CreateBitmap", gdih.}
#proc createBitmapIndirect*(pbm: ptr Bitmap): Handle
                          #{.importc: "CreateBitmapIndirect", gdih.}
proc createCompatibleDc*(hdc: Handle): Handle {.importc: "CreateCompatibleDC", gdih.}
proc getObject*(handle: Handle, c: int, pv: pointer): int
               {.importc: "GetObject", gdih.}
proc setPixel*(hdc: Handle, x, y: int, color: ColorRef): ColorRef
              {.importc: "SetPixel", gdih.}
proc ellipse*(hdc: Handle, left, top, right, bottom: int): WINBOOL
             {.importc: "Ellipse", gdih.}
proc roundRect*(hdc: Handle, left, top, right, bottom, width, height: int): WINBOOL
               {.importc: "RoundRect", gdih.}
proc chord*(hdc: Handle, x1, y1, x2, y2, x3, y3, x4, y4: int): WINBOOL
           {.importc: "Chord", gdih.}
template L*(s: string): WideCString = newWideCString s
proc sampleWindow*(classname, label: WideCString, cb: WndCb) =
  var msg: Message
  var hinstance = getModuleHandle("")
  var wc = WndClassW(
    style: CS_HREDRAW or CS_VREDRAW,
    className: classname,
    instance: hinstance,
    background: getSysColorBrush COLOR_3DFACE,
    wndProc: cb,
    cursor: loadCursor(0, idcArrow),
    icon: loadIcon(0, idiApplication),
  )
  registerClassW(addr wc)
  var hwnd = createWindowW(wc.className, label,
                           wsOverlappedWindow or wsVisible,
                           100, 100, 350, 250, 0, 0, nil, nil)
  showWindow(hwnd, 1)
  updateWindow(hwnd)
  var ptrmsg = addr msg
  while getMessage(ptrmsg, 0, 0, 0):
    translateMessage(ptrmsg)
    dispatchMessage(ptrmsg)

when isMainModule:
  proc wndProc(hwnd: Handle, msg: uint, wparam: cuint, lparam: cint): ptr int {.cdecl.} =
    case msg
    of WM_DESTROY:
      postQuitMessage 0
    else:
      discard
    defWindowProcW(hwnd, msg, wparam, lparam)

  let csname = newWideCString "Window"
  sampleWindow(csname, csname, wndProc)
