package main

import (
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Border")
	label1 := widget.NewLabel("Label 1")
	label2 := widget.NewLabel("Label 2")
	value1 := widget.NewLabel("Value")
	value2 := widget.NewLabel("Something")
	content := container.New(layout.NewFormLayout(), label1, value1, label2, value2)
	w.SetContent(content)
	w.ShowAndRun()
}
