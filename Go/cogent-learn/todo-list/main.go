package main

import (
	"cogentcore.org/core/core"
	"cogentcore.org/core/events"
	"cogentcore.org/core/icons"
)

type Item struct {
	Done bool `display:"checkbox"`
	Task string
}

func main() {
	b := core.NewBody()
	items := []Item{{Task: "Code"}, {Task: "Eat"}}
	var table *core.Table
	core.NewButton(b).
		SetText("Add").
		SetIcon(icons.Add).
		OnClick(func(e events.Event) {
			table.NewAt(0)
		})
	table = core.NewTable(b).SetSlice(&items)
	b.RunMainWindow()
}
