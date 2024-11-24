package main

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/theme"
)

func main() {
	a := app.New()
	w := a.NewWindow("Circle canvas")
	img := canvas.NewImageFromResource(theme.FyneLogo())
	img.FillMode = canvas.ImageFillOriginal
	w.SetContent(img)
	w.Resize(fyne.NewSize(100, 100))
	w.ShowAndRun()
}
