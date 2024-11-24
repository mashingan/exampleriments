package main

import (
	"fmt"

	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
)

func main() {
	myapp := app.New()
	myw := myapp.NewWindow("Loop")
	myw.SetContent(widget.NewLabel("Hehe"))
	myw.Show()
	myapp.Run()
	tidyUp()
}

func tidyUp() {
	fmt.Println("Exited")
}
