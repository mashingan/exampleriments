package main

import (
	"image/color"
	"time"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
)

func main() {
	a := app.New()
	w := a.NewWindow("animation")
	obj := canvas.NewRectangle(color.Black)
	obj.Resize(fyne.NewSize(50, 50))
	w.SetContent(container.NewWithoutLayout(obj))

	red := color.NRGBA{R: 0xff, A: 0xff}
	blue := color.NRGBA{B: 0xff, A: 0xff}
	toggle := false
	go func() {
		for {
			var startcolor, endcolor color.Color
			if toggle {
				startcolor = blue
				endcolor = red
			} else {
				startcolor = red
				endcolor = blue
			}
			toggle = !toggle
			coloranim := canvas.NewColorRGBAAnimation(startcolor, endcolor, time.Second*2, func(c color.Color) {
				obj.FillColor = c
				canvas.Refresh(obj)
			})
			coloranim.AutoReverse = true
			moveanim := canvas.NewPositionAnimation(fyne.NewPos(0, 0), fyne.NewPos(200, 0), time.Second, obj.Move)
			moveanim.AutoReverse = true
			coloranim.Start()
			moveanim.Start()
			time.Sleep(time.Second * 2)
		}
	}()
	w.Resize(fyne.NewSize(250, 50))
	w.SetPadded(false)
	w.ShowAndRun()
}
