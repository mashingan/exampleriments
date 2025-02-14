package main

import (
	"image"
	"image/color"
	"log"
	"os"
	"time"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/io/input"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
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
	// theme := material.NewTheme()
	var ops op.Ops
	gof, err := os.Open("golang_logo.png")
	if err != nil {
		log.Println(err)
		return err
	}
	defer gof.Close()
	img, _, err := image.Decode(gof)
	if err != nil {
		log.Println(err)
		return err
	}
	for {
		switch e := window.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			ops.Reset()
			drawRedRect(&ops)
			drawRedRectNPixelsRight(&ops, 110, 0)
			redButtonBackground(&ops)
			triangles(&ops, maroon)
			strokeRect(&ops)
			strokeTriangle(&ops)
			overlappingRect(&ops)
			fiveRectMacro(&ops)
			drawProgressBar(&ops, e.Source, time.Now())
			drawWithCache(&ops)
			drawImage(&ops, img)
			e.Frame(&ops)
		}
	}
}

var (
	red    = color.NRGBA{R: 0xff, A: 0xff}
	maroon = color.NRGBA{R: 0x80, A: 0xff}
	blue   = color.NRGBA{B: 0xff, A: 0xff}
	green  = color.NRGBA{G: 0xff, A: 0xff}
)

func drawRedRect(ops *op.Ops, color ...color.NRGBA) {
	defer clip.Rect{Max: image.Pt(100, 100)}.Push(ops).Pop()
	thecolor := red
	if len(color) > 0 {
		thecolor = color[0]

	}
	paint.ColorOp{Color: thecolor}.Add(ops)
	paint.PaintOp{}.Add(ops)
}

func drawRedRectNPixelsRight(ops *op.Ops, x, y int) {
	defer op.Offset(image.Pt(x, y)).Push(ops).Pop()
	drawRedRect(ops, maroon)
}

func redButtonBackground(ops *op.Ops) {
	const r = 10 // roundness
	bounds := image.Rect(0, 0, 100, 100)
	defer op.Offset(image.Pt(0, 110)).Push(ops).Pop()
	defer clip.RRect{Rect: bounds, SE: r, SW: r, NW: r, NE: r}.Push(ops).Pop()
	paint.ColorOp{Color: blue}.Add(ops)
	paint.PaintOp{}.Add(ops)
	// rr.Pop()
}

func triangles(ops *op.Ops, color color.NRGBA) {
	// off := op.Offset(image.Pt(110, 110)).Push(ops)
	defer op.Offset(image.Pt(110, 110)).Push(ops).Pop()
	var path clip.Path
	path.Begin(ops)
	path.Move(f32.Pt(50, 0))
	path.Quad(f32.Pt(0, 90), f32.Pt(50, 100))
	path.Line(f32.Pt(-100, 0))
	path.Line(f32.Pt(50, -100))
	defer clip.Outline{Path: path.End()}.Op().Push(ops).Pop()
	paint.ColorOp{Color: color}.Add(ops)
	paint.PaintOp{}.Add(ops)
	// off.Pop()
}

func strokeRect(ops *op.Ops) {
	const r = 10
	bounds := image.Rect(20, 20, 80, 80)
	defer op.Offset(image.Pt(0, 210)).Push(ops).Pop()
	rrect := clip.RRect{Rect: bounds, SE: r, SW: r, NE: r, NW: r}
	paint.FillShape(ops, red,
		clip.Stroke{
			Path:  rrect.Path(ops),
			Width: 4,
		}.Op())
}

func strokeTriangle(ops *op.Ops) {
	defer op.Offset(image.Pt(110, 210)).Push(ops).Pop()
	var path clip.Path
	path.Begin(ops)
	path.MoveTo(f32.Pt(30, 30))
	path.LineTo(f32.Pt(70, 30))
	path.LineTo(f32.Pt(50, 70))
	path.Close()
	paint.FillShape(ops, green,
		clip.Stroke{
			Path:  path.End(),
			Width: 4,
		}.Op())
}

func overlappingRect(ops *op.Ops) {
	defer op.Offset(image.Pt(0, 310)).Push(ops).Pop()
	cl := clip.Rect{Max: image.Pt(100, 50)}.Push(ops)
	paint.ColorOp{Color: maroon}.Add(ops)
	paint.PaintOp{}.Add(ops)
	cl.Pop()

	cl = clip.Rect{Max: image.Pt(50, 100)}.Push(ops)
	paint.ColorOp{Color: green}.Add(ops)
	paint.PaintOp{}.Add(ops)
	cl.Pop()
}

func fiveRectMacro(ops *op.Ops) {
	defer op.Offset(image.Pt(110, 310)).Push(ops).Pop()
	macro := op.Record(ops)
	drawRedRect(ops)
	c := macro.Stop()
	for i := 0; i < 5; i++ {
		c.Add(ops)
		op.Offset(image.Pt(110, 20)).Add(ops)
	}
}

var startTime = time.Now()
var duration = 10 * time.Second

func drawProgressBar(ops *op.Ops, source input.Source, now time.Time) {
	defer op.Offset(image.Pt(0, 420)).Push(ops).Pop()
	elapsed := now.Sub(startTime)
	progress := elapsed.Seconds() / duration.Seconds()
	if progress < 1 {
		source.Execute(op.InvalidateCmd{})
	} else {
		progress = 1
	}

	const r = 10
	bounds := image.Rect(0, 0, 200, 20)
	rrect := clip.RRect{Rect: bounds, SE: r, SW: r, NE: r, NW: r}
	paint.FillShape(ops, green,
		clip.Stroke{
			Path:  rrect.Path(ops),
			Width: 4,
		}.Op())

	width := 200 * float32(progress)
	// defer clip.Rect{Max: image.Pt(int(width), 20)}.Push(ops).Pop()
	bounds = image.Rect(0, 0, int(width), 20)
	defer clip.RRect{Rect: bounds, SE: r, SW: r, NE: r, NW: r}.Push(ops).Pop()
	paint.ColorOp{Color: maroon}.Add(ops)
	paint.PaintOp{}.Add(ops)
}

func drawWithCache(ops *op.Ops) {
	cache := new(op.Ops)
	macro := op.Record(cache)

	defer op.Offset(image.Pt(0, 450)).Push(ops).Pop()
	cl := clip.Rect{Max: image.Pt(100, 100)}.Push(cache)
	paint.ColorOp{Color: green}.Add(cache)
	paint.PaintOp{}.Add(cache)
	cl.Pop()
	call := macro.Stop()
	call.Add(ops)
}

func drawImage(ops *op.Ops, img image.Image) {
	defer op.Offset(image.Pt(0, 560)).Push(ops).Pop()
	imgOp := paint.NewImageOp(img)
	imgOp.Filter = paint.FilterNearest
	imgOp.Add(ops)
	op.Affine(f32.Affine2D{}.Scale(f32.Pt(0, 0), f32.Pt(0.1, 0.1))).Add(ops)
	paint.PaintOp{}.Add(ops)
}
