package main

import (
	"strconv"

	"cogentcore.org/core/core"
	"cogentcore.org/core/events"
	"cogentcore.org/core/tree"
)

func main() {
	b := core.NewBody()
	number := 3
	spinner := core.Bind(&number, core.NewSpinner(b)).SetMin(0)
	buttons := core.NewFrame(b)
	buttons.Maker(func(p *tree.Plan) {
		for i := range number {
			tree.AddAt(p, strconv.Itoa(i), func(w *core.Button) {
				w.SetText(strconv.Itoa(i))
			})
		}
	})
	spinner.OnChange(func(e events.Event) {
		buttons.Update()
	})
	b.RunMainWindow()
}
