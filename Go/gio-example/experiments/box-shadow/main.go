package main

import (
	"image"
	"image/color"
	"log"
	"os"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/unit"
)

func main() {
	go func() {
		window := new(app.Window)
		window.Option(app.Size(unit.Dp(400), unit.Dp(400)))
		if err := loop(window); err != nil {
			log.Fatal(err)
		}
		os.Exit(0)
	}()
	app.Main()
}

func loop(w *app.Window) error {
	var ops op.Ops
	for {
		switch e := w.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			ops.Reset()
			gtx := app.NewContext(&ops, e)
			colorBox(gtx)
			cb2(gtx)
			drawGradient(gtx)
			plainred(gtx)
			e.Frame(gtx.Ops)
		}
	}
}

var (
	red    = color.NRGBA{R: 0xff, A: 0xff}
	maroon = color.NRGBA{R: 0x80, A: 0xff}
	// shadow = color.NRGBA{R: 0xff, B: 0xff, G: 0xff, A: 0x11}
	black  = color.NRGBA{A: 0xff}
	shadow = color.NRGBA{A: 0x44}
)

func colorBox(gtx layout.Context, color ...color.NRGBA) layout.Dimensions {
	return layout.UniformInset(unit.Dp(10)).Layout(gtx,
		func(gtx layout.Context) layout.Dimensions {
			return layout.Background{}.Layout(gtx,
				func(gtx layout.Context) layout.Dimensions {
					defer clip.Rect{Max: image.Pt(102, 102)}.Push(gtx.Ops).Pop()
					paint.ColorOp{Color: shadow}.Add(gtx.Ops)
					paint.PaintOp{}.Add(gtx.Ops)
					return layout.Dimensions{Size: gtx.Constraints.Max}
				},
				func(gtx layout.Context) layout.Dimensions {
					return layout.UniformInset(unit.Dp(1)).Layout(gtx,
						func(gtx layout.Context) layout.Dimensions {
							defer clip.Rect{Max: image.Pt(100, 100)}.Push(gtx.Ops).Pop()
							thecolor := red
							if len(color) > 0 {
								thecolor = color[0]

							}
							paint.ColorOp{Color: thecolor}.Add(gtx.Ops)
							paint.PaintOp{}.Add(gtx.Ops)
							return layout.Dimensions{Size: gtx.Constraints.Max}

						})
				})
		})
}

func plainred(gtx layout.Context) layout.Dimensions {
	defer op.Offset(image.Pt(10, 120)).Push(gtx.Ops).Pop()
	defer clip.Rect{Max: image.Pt(100, 100)}.Push(gtx.Ops).Pop()
	paint.ColorOp{Color: red}.Add(gtx.Ops)
	paint.PaintOp{}.Add(gtx.Ops)
	return layout.Dimensions{Size: image.Pt(100, 100)}
}

func cb2(gtx layout.Context, color ...color.NRGBA) layout.Dimensions {
	defer op.Offset(image.Pt(120, 10)).Push(gtx.Ops).Pop()
	var cl clip.Stack
	const r = 10
	bounds := image.Rect(0, 0, 102, 102)
	rrect := clip.RRect{Rect: bounds, SE: r, SW: r, NE: r, NW: r}
	// cl = clip.Rect{Max: image.Pt(104, 104)}.Push(gtx.Ops)
	sd := black
	// sd.A = 0xa0
	paint.FillShape(gtx.Ops, sd,
		clip.Stroke{
			Path:  rrect.Path(gtx.Ops),
			Width: 1,
		}.Op())
	// paint.ColorOp{Color: sd}.Add(gtx.Ops)
	// paint.PaintOp{}.Add(gtx.Ops)
	// cl.Pop()
	thecolor := maroon
	if len(color) > 0 {
		thecolor = color[0]
	}
	defer op.Offset(image.Pt(1, 1)).Push(gtx.Ops).Pop()
	// cl = clip.RRect{Rect: image.Rect(0, 0, 100, 100), NW: r, NE: r, SE: r, SW: r}.Push(gtx.Ops)
	bounds = image.Rect(0, 0, 100, 100)
	cl = clip.RRect{Rect: bounds, NW: r, NE: r, SE: r, SW: r}.Push(gtx.Ops)
	// cl = clip.Rect{Max: image.Pt(100, 100)}.Push(gtx.Ops)
	paint.ColorOp{Color: thecolor}.Add(gtx.Ops)
	paint.PaintOp{}.Add(gtx.Ops)
	cl.Pop()
	return layout.Dimensions{Size: image.Pt(100, 100)}
}

func drawGradient(gtx layout.Context) layout.Dimensions {
	defer op.Offset(image.Pt(240, 10)).Push(gtx.Ops).Pop()
	pt := image.Pt(103, 100)
	cl := clip.Rect{Max: pt}.Push(gtx.Ops)
	// paint.ColorOp{Color: black}.Add(gtx.Ops)
	white := color.NRGBA{R: 0xff, G: 0xff, B: 0xff, A: 0xff}
	paint.LinearGradientOp{Stop1: f32.Pt(20, 00), Color1: black, Stop2: f32.Pt(80, 0), Color2: white}.Add(gtx.Ops)
	paint.PaintOp{}.Add(gtx.Ops)
	cl.Pop()
	// cl = clip.Rect{Max: image.Pt(100, 100)}.Push(gtx.Ops)
	// paint.ColorOp{Color: maroon}.Add(gtx.Ops)
	// paint.PaintOp{}.Add(gtx.Ops)
	// cl.Pop()
	return layout.Dimensions{Size: pt}
}
