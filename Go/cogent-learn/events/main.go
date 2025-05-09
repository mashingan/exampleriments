package main

import (
	"fmt"

	"cogentcore.org/core/core"
	"cogentcore.org/core/events"
	"cogentcore.org/core/icons"
)

func main() {
	b := core.NewBody("Events")
	clicks := 0
	txt := core.NewText(b).SetText(fmt.Sprint("Click: ", clicks))
	bt := core.NewButton(b).SetText("Increment!").SetIcon(icons.Add)
	bt.OnClick(func(e events.Event) {
		core.MessageSnackbar(b, fmt.Sprint("Button clicked at ", e.Pos()))
		clicks++
		txt.SetText(fmt.Sprint("Click: ", clicks))

	})
	core.NewButton(b).SetText("Decrement!").SetIcon(icons.ArrowDownward).OnClick(
		func(e events.Event) {

			core.MessageSnackbar(b, fmt.Sprint("Button clicked at ", e.Pos()))
			clicks--
			txt.SetText(fmt.Sprint("Click: ", clicks))

		})
	core.NewButton(b).SetText("Reset").SetIcon(icons.Reset).OnClick(
		func(e events.Event) {
			core.MessageSnackbar(b, fmt.Sprint("Button clicked at ", e.Pos()))
			clicks = 0
			txt.SetText(fmt.Sprint("Click: ", clicks))

		})
	b.RunMainWindow()
}
