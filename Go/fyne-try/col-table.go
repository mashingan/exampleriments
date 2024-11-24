package main

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
)

func main() {
	data := [][]string{
		{"top left", "top right"},
		{"bottom left", "bottom right"},
	}
	a := app.New()
	w := a.NewWindow("Table")
	list := widget.NewTable(
		func() (int, int) { return len(data), len(data[0]) },
		func() fyne.CanvasObject { return widget.NewLabel("wide content") },
		func(tci widget.TableCellID, co fyne.CanvasObject) {
			co.(*widget.Label).SetText(data[tci.Row][tci.Col])
		},
	)
	w.SetContent(list)
	w.ShowAndRun()
}
