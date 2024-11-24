package main

import (
	"log"
	"time"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("List")
	data := []string{"a", "string", "list"}
	list := widget.NewList(func() int {
		return len(data)
	}, func() fyne.CanvasObject {
		return widget.NewProgressBar()
	}, func(lii widget.ListItemID, co fyne.CanvasObject) {
		co.(*widget.ProgressBar).TextFormatter = func() string {
			return data[lii]
		}
	})
	list.OnSelected = func(id widget.ListItemID) {
		log.Printf("selected id: %d with data: %s", id, data[id])
	}
	w.SetContent(list)
	w.ShowAndRun()
}
