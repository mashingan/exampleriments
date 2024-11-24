package main

import (
	"log"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Entry")

	check := widget.NewCheck("Optional", func(b bool) {
		log.Println("check set to", b)
	})
	radio := widget.NewRadioGroup([]string{"option 1", "option 2"}, func(s string) {
		log.Println("Radio set to", s)
	})
	combo := widget.NewSelect([]string{"Option 1", "Option 2"}, func(s string) {
		log.Println("combo selected to", s)
	})
	content := container.NewVBox(check, radio, combo)
	w.SetContent(content)
	w.ShowAndRun()
}
