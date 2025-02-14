package main

import (
	"image"
	"image/color"
	"log"
	"os"

	"gioui.org/app"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	go func() {
		window := new(app.Window)
		window.Option(app.Title("Layout"))
		if err := run(window); err != nil {
			log.Fatal(err)
		}
		os.Exit(0)
	}()
	app.Main()
}

func run(window *app.Window) error {
	var ops op.Ops
	for {
		switch e := window.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			gtx := app.NewContext(&ops, e)
			listing(gtx)
			e.Frame(gtx.Ops)
		}
	}
}

func colorBox(gtx layout.Context, size image.Point, color color.NRGBA) layout.Dimensions {
	defer clip.Rect{Max: size}.Push(gtx.Ops).Pop()
	paint.ColorOp{Color: color}.Add(gtx.Ops)
	paint.PaintOp{}.Add(gtx.Ops)
	return layout.Dimensions{Size: size}
}

var list = layout.List{}

func listing(gtx layout.Context) layout.Dimensions {
	return list.Layout(gtx, 100, func(gtx layout.Context, index int) layout.Dimensions {
		col := color.NRGBA{R: byte(index * 20), G: 0x20, B: 0x20, A: 0xff}
		return colorBox(gtx, image.Pt(20, 100), col)
	})
}
