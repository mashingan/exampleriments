package main

import (
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/data/binding"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Two-ways")
	s := binding.NewString()
	label := widget.NewLabelWithData(s)
	entry := widget.NewEntryWithData(s)
	content := container.NewVBox(label, entry)
	w.SetContent(content)
	w.ShowAndRun()
}
