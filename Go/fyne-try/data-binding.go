package main

import (
	"time"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/data/binding"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Data Binding")

	str := binding.NewString()
	go func() {
		dots := "....."
		for i := len(dots); i >= 0; i-- {
			str.Set(dots[:i])
			time.Sleep(time.Second)
		}
	}()

	w.SetContent(widget.NewLabelWithData(str))
	w.ShowAndRun()
}
