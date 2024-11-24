package main

import (
	"image/color"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/theme"
)

func main() {
	a := app.New()
	w := a.NewWindow("Theme")
	a.Settings().SetTheme(&mytheme{})
	//w.SetContent(content)
	w.ShowAndRun()
}

type mytheme struct{ fyne.Theme }

var _ fyne.Theme = (*mytheme)(nil)

func (m mytheme) Color(name fyne.ThemeColorName, variant fyne.ThemeVariant) color.Color {
	if name == theme.ColorNameBackground {
		if variant == theme.VariantLight {
			return color.White
		}
		return color.Black
	}
	return theme.DefaultTheme().Color(name, variant)
}

var homebytes = []byte("")

func (m mytheme) Icon(name fyne.ThemeIconName) fyne.Resource {
	if name == theme.IconNameHome {
		fyne.NewStaticResource("myHome", homebytes)
	}
	return theme.DefaultTheme().Icon(name)
}
