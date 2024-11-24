package main

import (
	"strconv"

	"cogentcore.org/core/core"
	"cogentcore.org/core/events"
)

func main() {
	b := core.NewBody()
	count := 0
	txt := core.Bind(&count, core.NewText(b))
	core.NewButton(b).SetText("Increment").OnClick(func(e events.Event) {
		count++
		txt.Update()
	})

	on := true
	bs := core.NewSwitch(b)
	core.Bind(&on, bs).OnChange(func(e events.Event) {
		// fmt.Printf("%#v\n", b.Styles)
		core.MessageSnackbar(b, "The switch is now "+strconv.FormatBool(on))
	})
	// bs.Styler(func(s *styles.Style) {
	// })
	b.RunMainWindow()
}
