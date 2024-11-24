package main

import (
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("異世界無双")
	l := widget.NewLabel("Hello 異世界")
	w.SetContent(l)
	w.ShowAndRun()
}
