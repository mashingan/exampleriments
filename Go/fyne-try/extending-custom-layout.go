package main

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Custom layout - diagonal")
	text1 := widget.NewLabel("topleft")
	text2 := widget.NewLabel("middle label")
	text3 := widget.NewLabel("bottomright")
	content := container.New(&diagonal{}, text1, text2, text3)
	w.SetContent(content)
	w.ShowAndRun()
}

type diagonal struct{}

func (d *diagonal) MinSize(objects []fyne.CanvasObject) fyne.Size {
	w, h := float32(0), float32(0)
	for _, o := range objects {
		childsize := o.MinSize()
		w += childsize.Width
		h += childsize.Height
	}
	return fyne.NewSize(w, h)
}

func (d *diagonal) Layout(objects []fyne.CanvasObject, containerSize fyne.Size) {
	pos := fyne.NewPos(0, containerSize.Height-d.MinSize(objects).Height)
	for _, o := range objects {
		size := o.MinSize()
		o.Resize(size)
		o.Move(pos)
		pos = pos.Add(fyne.NewPos(size.Width, size.Height))
	}
}
