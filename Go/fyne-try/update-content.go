package main

import (
	"time"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
)

func main() {
	myapp := app.New()
	myw := myapp.NewWindow("Loop")
	clock := widget.NewLabel("")
	myw.SetContent(clock)
	updateTime(clock)
	go func() {
		for {
			<-time.Tick(time.Second)
			updateTime(clock)
		}
	}()
	myw.ShowAndRun()
}

func updateTime(clock *widget.Label) {
	clock.SetText(time.Now().Format("Time: 03:04:05"))
}
