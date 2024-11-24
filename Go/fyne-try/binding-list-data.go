package main

import (
	"fmt"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/data/binding"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("List data")

	data := binding.BindStringList(&[]string{"Item 1", "Item 2", "Item 3"})
	list := widget.NewListWithData(data,
		func() fyne.CanvasObject { return widget.NewLabel("template") },
		func(di binding.DataItem, co fyne.CanvasObject) {
			co.(*widget.Label).Bind(di.(binding.String))
		})
	add := widget.NewButton("Append", func() {
		val := fmt.Sprintf("Item %d", data.Length()+1)
		data.Append(val)
	})
	list.OnSelected = func(id widget.ListItemID) {
		item, err := data.GetItem(id)
		if err != nil {
			return
		}
		obj, ok := item.(binding.String)
		if !ok {
			return
		}
		str, _ := obj.Get()
		obj.Set(fmt.Sprintf("%s selected", str))
	}
	list.OnUnselected = func(id widget.ListItemID) {
		item, err := data.GetItem(id)
		if err != nil {
			return
		}
		obj, ok := item.(binding.String)
		if !ok {
			return
		}
		obj.Set(fmt.Sprintf("Item %d", id+1))

	}
	content := container.NewBorder(nil, add, nil, nil, list)
	w.SetContent(content)
	w.ShowAndRun()
}
