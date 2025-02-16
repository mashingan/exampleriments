package main

import (
	"image/color"
	"os"

	"gioui.org/app"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/component"
	"golang.org/x/exp/shiny/materialdesign/icons"
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
	var (
		ops   op.Ops
		modal = component.NewModal()
		th    = material.NewTheme()
		ab    = component.NewAppBar(modal)
	)
	navIcon, _ := widget.NewIcon(icons.NavigationApps)
	ab.NavigationIcon = navIcon
	ab.Title = "Nav"
	for {
		switch e := w.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			gtx := app.NewContext(&ops, e)
			thelayout(gtx, th, ab)
			e.Frame(gtx.Ops)
		}
	}
}

func thelayout(gtx layout.Context, th *material.Theme, ab *component.AppBar) layout.Dimensions {
	abar := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return ab.Layout(gtx, th, "Menu", "Actions")
	})
	layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		abar,
		layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
			defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
			paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
			paint.PaintOp{}.Add(gtx.Ops)
			return layout.Dimensions{Size: gtx.Constraints.Max}
		}))
	return layout.Dimensions{Size: gtx.Constraints.Max}
}
