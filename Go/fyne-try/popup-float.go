package main

import (
	"image/color"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/data/binding"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("float popup")
	data := binding.BindStringList(&[]string{"Item 1", "Item 2", "Item 3"})
	list := widget.NewListWithData(data,
		func() fyne.CanvasObject {
			return widget.NewLabel("template")
		}, func(di binding.DataItem, co fyne.CanvasObject) {
			co.(*widget.Label).Bind(di.(binding.String))
		})
	cvs := w.Canvas()
	float := widget.NewPopUp(
		canvas.NewText("+", color.NRGBA{R: 0xff, A: 0xff}),
		cvs,
	)
	content := container.NewBorder(list, float, nil, nil)
	list.Move(fyne.NewPos(0, 0))
	w.Resize(fyne.NewSize(300, 300))
	float.Move(w.Content().Position().SubtractXY(20, 20))
	w.SetContent(content)
	w.ShowAndRun()
}
