package main

import (
	"image/color"
	"log"
	"os"

	"gioui.org/app"
	"gioui.org/op"
	"gioui.org/op/paint"
)

func main() {
	title := "windowing"
	var window app.Window
	window.Option(app.Title(title))
	var ops op.Ops
	for {
		switch e := window.Event().(type) {
		case app.DestroyEvent:
			log.Println(e.Err)
			os.Exit(0)
		case app.FrameEvent:
			ops.Reset()
			// draw(&ops)
			e.Frame(&ops)
		}
	}
}

func draw(ops *op.Ops) {
	red := color.NRGBA{R: 0xff, A: 0xff}
	paint.ColorOp{Color: red}.Add(ops)
}
