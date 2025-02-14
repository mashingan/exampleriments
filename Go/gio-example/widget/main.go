package main

import (
	"image"
	"image/color"
	"log"
	"os"

	"gioui.org/app"
	"gioui.org/io/event"
	"gioui.org/io/pointer"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	go func() {
		window := new(app.Window)
		window.Option(app.Title("Widget"))
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
			log.Println(e.Err)
			return e.Err
		case app.FrameEvent:
			gtx := app.NewContext(&ops, e)
			draw(gtx)
			e.Frame(gtx.Ops)
		}
	}
}

type btnVisual struct {
	pressed bool
}

var bv = btnVisual{}

func draw(gtx layout.Context) {
	bv.Layout(gtx)
}

func (b *btnVisual) Layout(gtx layout.Context) layout.Dimensions {
	area := clip.Rect(image.Rect(0, 0, 100, 100)).Push(gtx.Ops)
	event.Op(gtx.Ops, b)
	for {
		ev, ok := gtx.Event(pointer.Filter{
			Target: b,
			Kinds:  pointer.Press | pointer.Release,
		})
		if !ok {
			break
		}

		e, ok := ev.(pointer.Event)
		if !ok {
			continue
		}

		switch e.Kind {
		case pointer.Press:
			b.pressed = true
		case pointer.Release:
			b.pressed = false
		}
	}
	area.Pop()

	col := color.NRGBA{R: 0x80, A: 0xff}
	if b.pressed {
		col = color.NRGBA{G: 0x80, A: 0xff}
	}
	return drawSquare(gtx.Ops, col)
}

func drawSquare(ops *op.Ops, color color.NRGBA) layout.Dimensions {
	defer clip.Rect{Max: image.Pt(100, 100)}.Push(ops).Pop()
	paint.ColorOp{Color: color}.Add(ops)
	paint.PaintOp{}.Add(ops)
	return layout.Dimensions{Size: image.Pt(100, 100)}
}
