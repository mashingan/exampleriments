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
	"gioui.org/unit"
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
			inset(gtx)
			e.Frame(gtx.Ops)
		}
	}
}

var (
	red = color.NRGBA{R: 0xc0, G: 0x40, B: 0x40, A: 0xff}
)

func colorBox(gtx layout.Context, size image.Point, color color.NRGBA) layout.Dimensions {
	defer clip.Rect{Max: size}.Push(gtx.Ops).Pop()
	paint.ColorOp{Color: color}.Add(gtx.Ops)
	paint.PaintOp{}.Add(gtx.Ops)
	return layout.Dimensions{Size: size}
}

func inset(gtx layout.Context) layout.Dimensions {
	return layout.UniformInset(unit.Dp(30)).Layout(gtx,
		func(gtx layout.Context) layout.Dimensions {
			return colorBox(gtx, gtx.Constraints.Max, red)
		})
}
