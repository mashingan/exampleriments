package main

import (
	"image/color"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/theme"
)

func main() {
	a := app.New()
	w := a.NewWindow("Max")
	img := canvas.NewImageFromResource(theme.FyneLogo())
	//img.FillMode = canvas.ImageFillOriginal
	text := canvas.NewText("Overlay", color.NRGBA{R: 0xff, A: 0xff})
	content := container.New(layout.NewMaxLayout(), img, text)
	w.SetContent(content)
	w.ShowAndRun()
}
