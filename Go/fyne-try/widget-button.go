package main

import (
	"log"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Button")
	content := container.New(layout.NewVBoxLayout(),
		widget.NewButton("click me", func() {
			log.Println("tapped")
		}),
		widget.NewButtonWithIcon("Home", theme.HomeIcon(), func() {
			log.Println("Go home!")
		}))
	w.SetContent(content)
	w.ShowAndRun()
}
