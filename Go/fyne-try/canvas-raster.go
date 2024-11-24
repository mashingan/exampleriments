package main

import (
	"image/color"
	"math/rand"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
)

func main() {
	a := app.New()
	w := a.NewWindow("Raster canvas")
	raster := canvas.NewRasterWithPixels(func(x, y, w, h int) color.Color {
		return color.NRGBA{
			uint8(rand.Intn(255)),
			uint8(rand.Intn(255)),
			uint8(rand.Intn(255)),
			0xff,
		}

	})
	w.SetContent(raster)
	w.Resize(fyne.NewSize(100, 100))
	w.ShowAndRun()
}
