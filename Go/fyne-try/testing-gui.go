package main

import (
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/widget"
)

func makeUI() (*widget.Label, *widget.Entry) {
	out := widget.NewLabel("Hello world")
	in := widget.NewEntry()
	in.OnChanged = func(s string) {
		out.SetText("Hello " + s)
	}
	return out, in
}

func main() {
	a := app.New()
	w := a.NewWindow("Hello person")
	w.SetContent(container.NewVBox(makeUI()))
	w.ShowAndRun()
}
