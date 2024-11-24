package main

import (
	"fmt"
	"image/color"
	"os"
	"strings"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
)

func main() {
	a := app.New()
	w := a.NewWindow("Line canvas")
	line := canvas.NewLine(color.White)
	if theme, ok := os.LookupEnv("FYNE_THEME"); ok &&
		strings.HasPrefix(strings.ToLower(theme), "light") {
		line.StrokeColor = color.Black
	}
	line.StrokeWidth = 5

	w.Resize(fyne.NewSize(100, 100))
	line.Position2.Y = line.Position1.Y
	fmt.Println("the line position is", line.Position())
	w.SetContent(line)
	w.ShowAndRun()
}
