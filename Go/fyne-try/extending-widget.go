package main

import (
	"log"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("tappable icon")
	w.SetContent(newMytapicon(theme.FyneLogo()))
	w.ShowAndRun()
}

type mytapicon struct {
	widget.Icon
}

func newMytapicon(res fyne.Resource) *mytapicon {
	result := &mytapicon{}
	result.ExtendBaseWidget(result)
	result.SetResource(res)
	return result
}

func (m *mytapicon) Tapped(_ *fyne.PointEvent) {
	log.Println("I have been tapped")
}

func (m *mytapicon) TappedSecondary(_ *fyne.PointEvent) {}
