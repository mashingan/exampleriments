package main

import (
	"fmt"
	"time"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/widget"
)

const (
	tenS    = "10 seconds"
	thirtyS = "30 seconds"
	oneMin  = "1 minute"
	tenD    = 10 * time.Second
	thirtyD = 30 * time.Second
	oneMinD = time.Minute
)

func main() {
	a := app.NewWithID("com.example.tutorial.preference")
	w := a.NewWindow("timeout")
	info := widget.NewLabel("")

	var timeout time.Duration
	timeoutSelector := widget.NewSelect(
		[]string{tenS, thirtyS, oneMin},
		func(s string) {
			switch s {
			case tenS:
				timeout = tenD
			case thirtyS:
				timeout = thirtyD
			case oneMin:
				timeout = oneMinD
			}
			a.Preferences().SetString("AppTimeout", s)
		},
	)
	timeoutSelector.SetSelected(a.Preferences().StringWithFallback("AppTimeout", tenS))
	ready := make(chan struct{}, 1)
	go func() {
		ready <- struct{}{}
		time.Sleep(timeout)
		a.Quit()
	}()
	runnings := 0
	go func() {
		<-ready
		info.SetText(fmt.Sprintf("timeout: %s, running: %02d", timeoutSelector.Selected, runnings))
		runnings++
		for range time.Tick(time.Second) {
			info.SetText(fmt.Sprintf("timeout: %s, running: %02d", timeoutSelector.Selected, runnings))
			runnings++
		}
	}()
	info.Resize(fyne.NewSize(70, 20))
	timeoutSelector.Move(fyne.NewPos(10, 30))
	timeoutSelector.Resize(fyne.NewSize(70, 20))
	w.Resize(fyne.NewSize(200, 200))
	content := container.NewWithoutLayout(info, timeoutSelector)
	w.SetContent(content)
	w.ShowAndRun()
}
