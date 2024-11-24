package main

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Overlay stack")
	cvs := w.Canvas()
	ov := cvs.Overlays()
	labels := [3]*widget.Label{}
	step := 5
	currentpos := 0
	for i, t := range [...]string{
		"This is first label",
		"This is second label",
		"This is third label",
	} {
		labels[i] = widget.NewLabel(t)
		ov.Add(labels[i])
		labels[i].Move(labels[i].Position().AddXY(float32(currentpos), float32(currentpos)))
		currentpos += step

	}
	w.Resize(fyne.NewSize(300, 200))
	w.ShowAndRun()
}
