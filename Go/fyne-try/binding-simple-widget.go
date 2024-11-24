package main

import (
	"time"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/data/binding"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Simple")
	s := binding.NewString()
	s.Set("Initial value")
	content := widget.NewLabelWithData(s)
	w.SetContent(content)
	go func() {
		time.Sleep(2 * time.Second)
		s.Set("New value after 2 seconds!")
	}()
	w.ShowAndRun()
}
