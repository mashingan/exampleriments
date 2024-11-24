package main

import (
	"image/color"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
)

func main() {
	a := app.New()
	w := a.NewWindow("Overlay stack")
	cvs := w.Canvas()
	ov := cvs.Overlays()
	text1 := canvas.NewText("This is a text", color.NRGBA{R: 0xff, A: 0xff})
	ov.Add(text1)
	text2 := canvas.NewText("This is a text", color.NRGBA{R: 0xff, A: 0x7f})
	ov.Add(text2)
	text2.Move(text1.Position().AddXY(2, 2))

	w.Resize(fyne.NewSize(300, 200))
	w.ShowAndRun()
}
