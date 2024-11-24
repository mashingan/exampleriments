package main

import (
	"image/color"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
)

func main() {
	a := app.New()
	w := a.NewWindow("Border")
	top := canvas.NewText("top bar", color.White)
	left := canvas.NewText("left", color.NRGBA{G: 0xff, A: 0xff})
	middle := canvas.NewText("content", color.NRGBA{R: 0xff, A: 0xff})
	content := container.NewBorder(top, nil, left, nil, middle)
	w.SetContent(content)
	w.ShowAndRun()
}
