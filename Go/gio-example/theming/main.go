package main

import (
	"log"
	"os"

	"gioui.org/app"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/widget"
	"gioui.org/widget/material"
)

func main() {
	go func() {
		window := new(app.Window)
		window.Option(app.Title("Theming"))
		if err := run(window); err != nil {
			log.Fatal(err)
		}
		os.Exit(0)
	}()
	app.Main()
}

func run(window *app.Window) error {
	theme := material.NewTheme()
	var ops op.Ops
	for {
		switch e := window.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			ops.Reset()
			gtx := app.NewContext(&ops, e)
			themedApplication(gtx, theme)
			e.Frame(gtx.Ops)
		}
	}
}

var isChecked widget.Bool

func themedApplication(gtx layout.Context, th *material.Theme) layout.Dimensions {
	var checkboxLabel string
	isChecked.Update(gtx)
	if isChecked.Value {
		checkboxLabel = "checked"
	} else {
		checkboxLabel = "not-checked"
	}
	return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(material.H3(th, "Hello world!").Layout),
		layout.Rigid(material.CheckBox(th, &isChecked, checkboxLabel).Layout))
}
