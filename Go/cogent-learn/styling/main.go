package main

import (
	"cogentcore.org/core/colors"
	"cogentcore.org/core/core"
	"cogentcore.org/core/styles"
	"cogentcore.org/core/styles/units"
)

func main() {
	b := core.NewBody("Styling")
	core.NewText(b).SetText("Bold Text").Styler(func(s *styles.Style) {
		s.Font.Weight = styles.WeightBold
	})
	core.NewButton(b).SetText("Success button").Styler(func(s *styles.Style) {
		s.Background = colors.Scheme.Success.Base
		s.Color = colors.Scheme.Success.On
	})
	core.NewFrame(b).Styler(func(s *styles.Style) {
		s.Min.Set(units.Dp(50))
		s.Background = colors.Scheme.Primary.Base
	})
	b.RunMainWindow()
}
