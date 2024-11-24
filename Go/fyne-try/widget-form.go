package main

import (
	"log"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Form")
	entry := widget.NewEntry()
	textArea := widget.NewMultiLineEntry()
	content := &widget.Form{
		Items: []*widget.FormItem{
			{Text: "Entry", Widget: entry},
		},
		OnSubmit: func() {
			log.Println("Form submitted:", entry.Text)
			log.Println("multiline:", textArea.Text)
			w.Close()
		},
	}
	content.Append("Text", textArea)

	w.SetContent(content)
	w.ShowAndRun()
}
