package main

import (
	"image/color"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
)

func main() {
	a := app.New()
	w := a.NewWindow("Grid wrap")
	text1 := canvas.NewText("1", color.White)
	text2 := canvas.NewText("2", color.NRGBA{G: 0xff, A: 0xff})
	text3 := canvas.NewText("3", color.NRGBA{R: 0xff, A: 0xff})
	grid := container.New(layout.NewGridWrapLayout(fyne.NewSize(50, 50)), text1, text2, text3)
	w.SetContent(grid)
	//w.Resize(fyne.NewSize(120, 75))
	w.ShowAndRun()
}
