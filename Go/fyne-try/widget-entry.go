package main

import (
	"log"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/widget"
)

// for widget.NewPasswordEntry is available in password-strength.go
func main() {
	a := app.New()
	w := a.NewWindow("Entry")
	input := widget.NewEntry()
	input.SetPlaceHolder("Enter text...")
	content := container.NewVBox(input, widget.NewButton("Save", func() {
		log.Println("Content was:", input.Text)
	}))
	w.SetContent(content)
	w.ShowAndRun()
}
