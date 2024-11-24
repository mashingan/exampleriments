package main

import (
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/data/binding"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Data conversion")
	f := binding.NewFloat()
	s := binding.FloatToString(f)
	f.Set(25)
	srt := binding.FloatToStringWithFormat(f, "%0.0f%%")
	content := container.NewVBox(
		widget.NewSliderWithData(0, 100.0, f),
		widget.NewLabelWithData(s),
		widget.NewLabelWithData(srt),
	)
	w.SetContent(content)
	w.ShowAndRun()
}
