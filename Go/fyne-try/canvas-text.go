package main

import (
	"image/color"
	"os"
	"strings"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
)

func main() {
	a := app.New()
	w := a.NewWindow("Text canvas")
	text := canvas.NewText("Text object", color.White)
	if theme, ok := os.LookupEnv("FYNE_THEME"); ok &&
		strings.HasPrefix(strings.ToLower(theme), "light") {
		text.Color = color.Black
	}
	text.Alignment = fyne.TextAlignCenter
	text.TextStyle = fyne.TextStyle{
		//Monospace: true,
		Italic: true,
	}

	w.SetContent(text)
	w.ShowAndRun()
}
