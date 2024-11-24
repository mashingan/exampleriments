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
	w := a.NewWindow("Box")
	text1 := canvas.NewText("Hello", color.White)
	text2 := canvas.NewText("there", color.NRGBA{G: 0xff, A: 0xff})
	text3 := canvas.NewText("right", color.NRGBA{R: 0xff, A: 0xff})
	content := container.New(layout.NewHBoxLayout(), text1, text2, layout.NewSpacer(), text3)
	text4 := canvas.NewText("centered", color.White)
	centered := container.New(layout.NewHBoxLayout(), layout.NewSpacer(), text4, layout.NewSpacer())
	w.SetContent(container.New(layout.NewVBoxLayout(), content, centered))
	w.ShowAndRun()
}
