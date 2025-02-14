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
	"gioui.org/widget/material"
)

func main() {
	go func() {
		window := new(app.Window)
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
			gtx := app.NewContext(&ops, e)
			exampleSplitVisua(gtx, theme)
			e.Frame(gtx.Ops)
		}
	}
}

type SplitVisual struct{}

func (s SplitVisual) Layout(gtx layout.Context,
	left, right layout.Widget) layout.Dimensions {
	leftsize := gtx.Constraints.Min.X / 2
	rightsize := gtx.Constraints.Min.X - leftsize
	{
		gtx := gtx
		gtx.Constraints = layout.Exact(image.Pt(leftsize, gtx.Constraints.Max.Y))
		left(gtx)
	}
	{
		gtx := gtx
		gtx.Constraints = layout.Exact(image.Pt(rightsize, gtx.Constraints.Max.Y))
		trans := op.Offset(image.Pt(leftsize, 0)).Push(gtx.Ops)
		right(gtx)
		trans.Pop()
	}
	return layout.Dimensions{Size: gtx.Constraints.Max}
}

var (
	red  = color.NRGBA{R: 255, G: 0, B: 0, A: 255}
	blue = color.NRGBA{R: 0, G: 0, B: 255, A: 255}
)

func exampleSplitVisua(gtx layout.Context, th *material.Theme) layout.Dimensions {
	return SplitVisual{}.Layout(gtx,
		func(gtx layout.Context) layout.Dimensions {
			return fillWithLabel(gtx, th, "Left", red)
		}, func(gtx layout.Context) layout.Dimensions {
			return fillWithLabel(gtx, th, "Right", blue)
		})
}

func fillWithLabel(gtx layout.Context, th *material.Theme,
	label string, color color.NRGBA) layout.Dimensions {
	defer clip.Rect{Max: image.Pt(100, 100)}.Push(gtx.Ops).Pop()
	paint.ColorOp{Color: color}.Add(gtx.Ops)
	paint.PaintOp{}.Add(gtx.Ops)
	return layout.Center.Layout(gtx, material.H3(th, label).Layout)
}
