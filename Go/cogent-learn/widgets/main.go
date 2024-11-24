package main

import (
	"cogentcore.org/core/core"
	"cogentcore.org/core/icons"
)

func main() {
	b := core.NewBody("Widget")
	bt := core.NewButton(b).SetText("Click me!").SetIcon(icons.Add)
	go func() {
		bt.SetText("New text")
	}()
	b.RunMainWindow()
}
