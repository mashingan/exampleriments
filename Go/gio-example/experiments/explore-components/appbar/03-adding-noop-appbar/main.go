package main

import (
	"os"

	"gioui.org/app"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/widget/material"
	"gioui.org/x/component"
)

func main() {
	go func() {
		window := new(app.Window)
		if err := loop(window); err != nil {
			panic(err)
		}
		os.Exit(0)
	}()
	app.Main()
}

func loop(w *app.Window) error {
	var ops op.Ops
	modal := component.NewModal()
	appbar := component.NewAppBar(modal)
	th := material.NewTheme()
	for {
		switch e := w.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			gtx := app.NewContext(&ops, e)
			thelayout(gtx, th, appbar)
			e.Frame(gtx.Ops)
		}
	}
}

func thelayout(gtx layout.Context, th *material.Theme, appbar *component.AppBar) layout.Dimensions {
	appbar.Layout(gtx, th, "Menu", "Actions")
	return layout.Dimensions{Size: gtx.Constraints.Max}
}
