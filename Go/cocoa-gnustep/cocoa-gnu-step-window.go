package main

import (
	"log/slog"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"structs"

	"github.com/ebitengine/purego"
	"golang.org/x/sys/windows"
)

const (
	NSApplicationActivationPolicyRegular = 0
	NSWindowStyleMaskTitled              = 1 << 0
	NSWindowStyleMaskClosable            = 1 << 1
	NSWindowStyleMaskMiniaturizable      = 1 << 2
	NSWindowStyleMaskResizeable          = 1 << 3
	NSBackingStoreBuffered               = 2
)

const (
	basedllpath = "d:/installer/gnustep/x64/release/bin"
	gnustep     = "gnustep-base-1_31.dll"
	gnubase     = "gnustep-corebase-0.dll"
	objcdll     = "objc.dll"
)

func main() {
	runtime.LockOSThread()
	objc := windows.NewLazyDLL(filepath.Join(basedllpath, objcdll)).Handle()
	_ = objc
	basedir, err := os.ReadDir(basedllpath)
	if err != nil {
		panic(err)
	}
	skipdll := []string{gnustep, gnubase, objcdll}
	for _, f := range basedir {
		fnamelower := strings.ToLower(f.Name())
		if !strings.HasSuffix(f.Name(), ".dll") || slices.Contains(skipdll, fnamelower) {
			continue
		}
		baseload := windows.NewLazyDLL(filepath.Join(basedllpath, f.Name()))
		if err := baseload.Load(); err != nil {
			slog.Info(f.Name())
			panic(err)
		}
	}
	gnustep := windows.NewLazyDLL(filepath.Join(basedllpath, gnustep)).Handle()
	_ = gnustep
	stepBaseLoad := windows.NewLazyDLL(filepath.Join(basedllpath, gnubase))
	if err := stepBaseLoad.Load(); err != nil {
		panic(err)
	}
	stepbase := stepBaseLoad.Handle()
	_ = stepbase
	type (
		Class = uintptr
		ID    = uintptr
		SEL   = uintptr
	)
	var (
		objc_getClass    func(name string) Class
		sel_registerName func(name string) SEL
		objc_msgSend     func(obj ID, cmd SEL, args ...any) ID
		NSLog            func(format ID, args ...any)
	)
	purego.RegisterLibFunc(&objc_getClass, objc, "objc_getClass")
	purego.RegisterLibFunc(&sel_registerName, objc, "sel_registerName")
	purego.RegisterLibFunc(&objc_msgSend, objc, "objc_msgSend")
	purego.RegisterLibFunc(&NSLog, gnustep, "NSLog")
	// nsapp := objc_msgSend(objc_getClass("NSApplication"), sel_registerName("sharedApplication"))
	// objc_msgSend(nsapp, sel_registerName("setActivationPolicy:"), NSApplicationActivationPolicyRegular)
	utf8str := sel_registerName("stringWithUTF8String:")
	nsStrCls := objc_getClass("NSString")
	wtitle := objc_msgSend(nsStrCls, utf8str, "Hello world 異世界!")
	fmts := objc_msgSend(nsStrCls, utf8str, "this is from windows: %@")
	NSLog(fmts, wtitle)

	// nsApp := objc.ID(objc.GetClass("NSApplication")).Send(objc.RegisterName("sharedApplication"))
	// nsApp.Send(objc.RegisterName("setActivationPolicy:"), NSApplicationActivationPolicyRegular)
	// allocSEL := objc.RegisterName("alloc")
	// wnd := objc.ID(objc.GetClass("NSWindow")).Send(allocSEL)
	// styleMask := NSWindowStyleMaskClosable | NSWindowStyleMaskTitled
	// wnd = wnd.Send(objc.RegisterName("initWithContentRect:styleMask:backing:defer:"),
	// 	NSMakeRect(0, 0, 320, 240),
	// 	styleMask,
	// 	NSBackingStoreBuffered,
	// 	false,
	// )

	// delegate, err := objc.RegisterClass("AppDelegate", objc.GetClass("NSObject"), nil, nil,
	// 	[]objc.MethodDef{
	// 		{
	// 			Cmd: objc.RegisterName("applicationShouldTerminateAfterLastWindowClosed:"),
	// 			Fn: func(self objc.ID, _cmd objc.SEL) bool {
	// 				return true

	// 			},
	// 		},
	// 	})
	// if err != nil {
	// 	slog.Error("Register class", "error", err)
	// 	return
	// }
	// delegateId := objc.ID(delegate).Send(allocSEL)
	// nsApp.Send(objc.RegisterName("setDelegate:"), delegateId)

	// utf8str := objc.RegisterName("stringWithUTF8String:")
	// wtitle := objc.ID(objc.GetClass("NSString")).Send(utf8str, title)
	// wnd.Send(objc.RegisterName("setTitle:"), wtitle)
	// wnd.Send(objc.RegisterName("makeKeyAndOrderFront:"), objc.ID(0))
	// wnd.Send(objc.RegisterName("center"))

	// hellogo := objc.ID(objc.GetClass("NSString")).Send(utf8str)
	// label := objc.ID(objc.GetClass("NSTextField")).Send(objc.RegisterName("labelWithString:"), hellogo)
	// label.Send(objc.RegisterName("setFrame:"), NSMakeRect(20, 180, 280, 24))
	// contentView := wnd.Send(objc.RegisterName("contentView"))
	// contentView.Send(objc.RegisterName("addSubView:"), label)

	// nsApp.Send(objc.RegisterName("activateIgnoringOtherApps:"), true)
	// nsApp.Send(objc.RegisterName("run"))
}

type NSPoint struct {
	_    structs.HostLayout
	X, Y float64
}

type NSSize struct {
	_             structs.HostLayout
	Width, Height float64
}

type NSRect struct {
	_      structs.HostLayout
	Origin NSPoint
	Size   NSSize
}

func NSMakeRect(x, y, width, height float64) NSRect {
	return NSRect{Origin: NSPoint{X: x, Y: y}, Size: NSSize{Width: width, Height: height}}
}
