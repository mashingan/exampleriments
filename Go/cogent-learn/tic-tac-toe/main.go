package main

import (
	"cogentcore.org/core/core"
	"cogentcore.org/core/events"
	"cogentcore.org/core/styles"
	"cogentcore.org/core/styles/units"
)

func main() {
	b := core.NewBody()
	current := "X"
	squares := [9]string{}
	status := core.NewText(b)
	status.Updater(func() {
		sets := [...][3]int{
			{0, 1, 2}, {3, 4, 5}, {6, 7, 8},
			{0, 3, 6}, {1, 4, 7}, {2, 5, 8},
			{0, 4, 8}, {2, 4, 6},
		}
		for _, set := range sets {
			if squares[set[0]] != "" && squares[set[0]] == squares[set[1]] &&
				squares[set[1]] == squares[set[2]] && squares[set[0]] == squares[set[2]] {
				status.SetText(squares[set[0]] + " win")
				current = ""
				return
			}
		}
		isDraw := true
		for _, s := range squares {
			if s == "" {
				isDraw = false
				break
			}
		}
		if isDraw {
			status.SetText("Draw. Press Reset to play again!")
			current = ""
			return
		}
		status.SetText("Next player: " + current)

	})
	grid := core.NewFrame(b)
	grid.Styler(func(s *styles.Style) {
		s.Display = styles.Grid
		s.Columns = 3
	})
	for i := range 9 {
		bt := core.NewButton(grid).SetType(core.ButtonAction)
		bt.Styler(func(s *styles.Style) {
			s.Border.Width.Set(units.Dp(1))
			s.Border.Radius.Zero()
			s.Min.Set(units.Em(2))
		})
		bt.OnClick(func(e events.Event) {
			if squares[i] != "" || current == "" {
				return
			}
			squares[i] = current
			if current == "X" {
				current = "O"
			} else {
				current = "X"
			}
			bt.Update()
			status.Update()
		})
		bt.Updater(func() {
			bt.SetText(squares[i])
		})
	}
	core.NewButton(b).SetText("Reset").OnClick(func(e events.Event) {
		squares = [9]string{}
		current = "X"
		grid.Update()
		status.Update()
	})
	b.RunMainWindow()
}
