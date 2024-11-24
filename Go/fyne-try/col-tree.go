package main

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
)

func main() {
	a := app.New()
	w := a.NewWindow("Tree")
	tree := widget.NewTree(
		func(tni widget.TreeNodeID) []widget.TreeNodeID {
			switch tni {
			case "":
				return []widget.TreeNodeID{"a", "b", "c"}
			case "a":
				return []widget.TreeNodeID{"a1", "a2"}
			}
			return []string{}
		},
		func(tni widget.TreeNodeID) bool {
			return tni == "" || tni == "a"
		},
		func(b bool) fyne.CanvasObject {
			if b {
				return widget.NewLabel("Branch template")
			}
			return widget.NewLabel("Leaf template")
		},
		func(tni widget.TreeNodeID, b bool, co fyne.CanvasObject) {
			text := tni
			if b {
				text += " branch"
			}
			co.(*widget.Label).SetText(text)
		},
	)
	w.SetContent(tree)
	w.ShowAndRun()
}
