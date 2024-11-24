package main

import (
	"fmt"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
)

func main() {
	mapp := app.New()
	w := mapp.NewWindow("Hello world 1")
	totalWindow := 0
	cont := widget.NewButton("press to new window", func() {
		totalWindow++
		w2 := mapp.NewWindow(fmt.Sprintf("new window %d", totalWindow))
		w2.SetContent(widget.NewLabel(fmt.Sprintf("This is the new window of number %d", totalWindow)))
		w2.Resize(fyne.NewSize(100, 100))
		w2.Show()

	})
	w.SetContent(cont)
	w.ShowAndRun()
}
