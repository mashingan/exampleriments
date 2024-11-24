package main

import (
	"image/color"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
)

func main() {
	a := app.New()
	w := a.NewWindow("Container and Layout")
	green := color.NRGBA{0, 180, 0, 255}
	text1 := canvas.NewText("Hello green", green)
	text2 := canvas.NewText("Hello green", green)
	content := container.NewWithoutLayout(text1, text2)
	w.SetContent(content)
	w.ShowAndRun()
}
