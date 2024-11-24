package main

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/widget"
	pwdvalid "github.com/wagslane/go-password-validator"
)

const minEntropyBits = 60

func main() {
	a := app.New()
	w := a.NewWindow("Pwd str")
	entry := widget.NewPasswordEntry()
	entry.Resize(fyne.NewSize(50, 20))
	label := widget.NewLabel("Password")
	label.Resize(fyne.NewSize(30, 20))
	info := widget.NewProgressBar()
	info.Max = minEntropyBits
	info.TextFormatter = func() string {
		if info.Value <= 30 {
			return "weak"
		} else if info.Value > 30 && info.Value <= 45 {
			return "medium"
		} else {
			return "strong"
		}
	}
	content := container.NewVBox(container.New(
		layout.NewFormLayout(), label, entry), info)
	entry.OnChanged = func(s string) {
		ent := pwdvalid.GetEntropy(s)
		info.SetValue(ent)
	}
	w.Resize(fyne.NewSize(300, 100))
	w.SetContent(content)
	w.ShowAndRun()
}
