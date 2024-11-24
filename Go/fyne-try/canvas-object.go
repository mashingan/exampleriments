package main

import (
	"image/color"
	"os"
	"strings"
	"time"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/layout"
)

func ease(m fyne.CanvasObject, dx, dy, step float32, duration time.Duration) {
	numd := int64(duration)
	pos := int64(0)
	dxdiff := dx / step
	dydiff := dy / step
	dur := numd / int64(step)
	newdx := m.Position().X
	newdy := m.Position().Y
	ready := make(chan struct{}, 1)
	for pos < numd {
		go func() {
			pos += dur
			newdx += dxdiff
			newdy += dydiff
			m.Move(fyne.NewPos(newdx, newdy))
			m.Refresh()
			m.Resize(fyne.NewSize(10, 10))
			ready <- struct{}{}
		}()
		time.Sleep(time.Duration(dur))
		<-ready
	}
}

func main() {
	a := app.New()
	win := a.NewWindow("Canvas")
	cvs := win.Canvas()

	blue := color.NRGBA{0, 0, 180, 255}
	green := color.NRGBA{0, 180, 0, 255}
	rect := canvas.NewRectangle(blue)
	cvs.SetContent(rect)
	cot := container.NewWithoutLayout()
	oktext := canvas.NewText("ok", color.White)
	if env, ok := os.LookupEnv("FYNE_THEME"); ok && strings.ToLower(env) == "light" {
		oktext.Color = color.Black
	}
	content := container.New(layout.NewVBoxLayout(), oktext, cvs.Content(), cot)
	toggle := true
	diffpos := float32(90)
	setContentToText(cot)

	go func() {
		for {
			if toggle {
				rect.FillColor = green
				ease(rect, diffpos, 0, 60, time.Second)
				setContentToCircle(cot)
			} else {
				rect.FillColor = blue
				ease(rect, -diffpos, 0, 60, time.Second)
				setContentToText(cot)
			}
			toggle = !toggle

		}
	}()
	win.SetContent(content)
	win.Resize(fyne.NewSize(100, 100))
	win.ShowAndRun()
}

func moveDown10px(c fyne.CanvasObject) {
	c.Move(fyne.NewPos(c.Position().X, c.Position().Y+10))
}

func setContentToText(c *fyne.Container) {
	green := color.NRGBA{0, 180, 0, 255}
	text := canvas.NewText("Text", green)
	text.TextStyle.Bold = true
	moveDown10px(text)
	c.RemoveAll()
	c.Add(text)
}

func setContentToCircle(c *fyne.Container) {
	red := color.NRGBA{0xff, 0x33, 0x33, 0xff}
	circle := canvas.NewCircle(color.White)
	if env, ok := os.LookupEnv("FYNE_THEME"); ok && strings.ToLower(env) == "light" {
		circle.FillColor = color.Black
	}
	circle.StrokeWidth = 4
	circle.StrokeColor = red
	circle.Resize(fyne.NewSize(30, 30))
	moveDown10px(circle)
	c.RemoveAll()
	c.Add(circle)
}
