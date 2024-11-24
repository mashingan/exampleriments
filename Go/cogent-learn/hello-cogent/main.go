package main

import (
	"cogentcore.org/core/core"
)

func main() {
	b := core.NewBody("App-Base")
	core.NewButton(b).SetText("Click me")
	b.RunMainWindow()

}
