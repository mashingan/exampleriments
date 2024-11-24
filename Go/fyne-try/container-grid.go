package main

import (
	"image/color"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
)

func main() {
	a := app.New()
	w := a.NewWindow("Grid")
	text1 := canvas.NewText("Hello", color.White)
	text2 := canvas.NewText("there", color.NRGBA{G: 0xff, A: 0xff})
	text3 := canvas.NewText("right", color.NRGBA{R: 0xff, A: 0xff})
	grid := container.New(layout.NewGridLayout(2), text1, text2, text3)
	w.SetContent(grid)
	w.ShowAndRun()
}
