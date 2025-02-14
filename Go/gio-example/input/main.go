package main

import (
	"image"
	"image/color"
	"log"
	"os"
	"sync"
	"time"

	"gioui.org/app"
	"gioui.org/io/event"
	"gioui.org/io/input"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
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
	var ops op.Ops
	go func() {
		changes := time.NewTicker(time.Second)
		defer changes.Stop()
		for t := range changes.C {
			updateOffset(int((t.Second() % 3) * 100))
			window.Invalidate()
		}
	}()
	for {
		switch e := window.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			ops.Reset()
			doButton(&ops, e.Source)
			moveButton(&ops, e.Source)
			doPointerTree(&ops, e.Source)
			op.Offset(image.Pt(readoffset(), 0)).Add(&ops)
			doButton(&ops, e.Source)
			e.Frame(&ops)
		}
	}
}

var (
	tag        = new(bool)
	pressed    = false
	tag2       = new(bool)
	moveBounds = image.Rect(100, 0, 200, 100)
	relative   image.Point
)

func doButton(ops *op.Ops, q input.Source) {
	const r = 10
	bounds := image.Rect(0, 0, 100, 100)
	defer clip.RRect{Rect: bounds, SE: r, SW: r, NW: r, NE: r}.Push(ops).Pop()
	event.Op(ops, tag)

	for {
		ev, ok := q.Event(pointer.Filter{
			Target: tag,
			Kinds:  pointer.Press | pointer.Release,
		})
		if !ok {
			break
		}

		if x, ok := ev.(pointer.Event); ok {
			// mousepos = x.Position
			switch x.Kind {
			case pointer.Press:
				log.Println("pressed green:", x.Position)
				pressed = true
			case pointer.Release:
				pressed = false
			}
		}
	}
	c := color.NRGBA{G: 0xff, A: 0xff}
	// if pressed && inPosition(mousepos, bounds) {
	if pressed {
		c.R = 0xff
		c.G = 0x00
	}
	paint.ColorOp{Color: c}.Add(ops)
	paint.PaintOp{}.Add(ops)
}

func moveButton(ops *op.Ops, q input.Source) {
	const r = 10
	defer clip.RRect{Rect: moveBounds, SE: r, SW: r, NW: r, NE: r}.Push(ops).Pop()
	event.Op(ops, tag2)

	for {
		ev, ok := q.Event(pointer.Filter{
			Target: tag2,
			Kinds:  pointer.Drag | pointer.Press | pointer.Release,
		})
		if !ok {
			break
		}

		if x, ok := ev.(pointer.Event); ok {
			switch x.Kind {
			case pointer.Press:
				relative = x.Position.Round().Sub(image.Pt(moveBounds.Min.X, moveBounds.Min.Y))
			case pointer.Drag:
				dxy := x.Position.Round().Sub(image.Pt(moveBounds.Min.X, moveBounds.Min.Y)).Sub(relative)
				moveBounds = moveBounds.Add(dxy)
				defer op.Offset(dxy).Push(ops).Pop()
			case pointer.Release:
			}
			// log.Println(x.Position)
		}
	}
	c := color.NRGBA{B: 0xff, A: 0xff}
	paint.ColorOp{Color: c}.Add(ops)
	paint.PaintOp{}.Add(ops)

}

var mvBtn struct {
	lock   sync.RWMutex
	offset int
}

func updateOffset(v int) {
	mvBtn.lock.Lock()
	defer mvBtn.lock.Unlock()
	mvBtn.offset = v
}

func readoffset() int {
	mvBtn.lock.RLock()
	defer mvBtn.lock.RUnlock()
	return mvBtn.offset
}

var (
	root, child1, child2 bool
)

func displayForTag(ops *op.Ops, tag *bool, rect clip.Rect) {
	event.Op(ops, tag)
	c := color.NRGBA{B: 0xff, A: 0xff}
	if *tag {
		c.R = 0xff
		c.B = 0x00
	}

	translucent := c
	translucent.A = 0x44
	paint.ColorOp{Color: translucent}.Add(ops)
	paint.PaintOp{}.Add(ops)

	defer clip.Stroke{
		Path:  rect.Path(),
		Width: 5,
	}.Op().Push(ops).Pop()
	paint.ColorOp{Color: c}.Add(ops)
	paint.PaintOp{}.Add(ops)
}

func doPointerTree(ops *op.Ops, q input.Source) {
	for _, tag := range []*bool{&root, &child1, &child2} {
		for {
			ev, ok := q.Event(pointer.Filter{
				Target: tag,
				Kinds:  pointer.Press | pointer.Release,
			})
			if !ok {
				break
			}
			x, ok := ev.(pointer.Event)
			if !ok {
				continue
			}

			switch x.Kind {
			case pointer.Press:
				*tag = true
			case pointer.Release:
				*tag = false
			}
		}
	}
	rootRect := clip.Rect(image.Rect(0, 110, 200, 310))
	rootArea := rootRect.Push(ops)
	displayForTag(ops, &root, rootRect)

	child1Rect := clip.Rect(image.Rect(25, 135, 175, 210))
	child1Area := child1Rect.Push(ops)
	displayForTag(ops, &child1, child1Rect)
	child1Area.Pop()

	child2Rect := clip.Rect(image.Rect(100, 135, 175, 285))
	child2Area := child2Rect.Push(ops)
	displayForTag(ops, &child2, child2Rect)
	child2Area.Pop()

	rootArea.Pop()
}
