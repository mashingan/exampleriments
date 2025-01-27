import winlean

{.pragma: winh, header: "<windows.h>".}

let
  mbAbortRetryIgnore {.importc: "MB_ABORTRETRYIGNORE", winh.}: cuint 
  mbOk {.importc: "MB_OK", winh.}: cuint 
  mbCancel {.importc: "MB_CANCEL", winh.}: cuint
  mbRetryCancel {.importc: "MB_RETRYCANCEL", winh.}: cuint
proc messageBox(hwnd: Handle, text, caption: cstring, flag: cuint): cint
  {.importc: "MessageBox", winh.}

discard messageBox(0, "Hello異世界", "Msg", mbOk)
