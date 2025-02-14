package applayout

import (
	"gioui.org/layout"
	"gioui.org/unit"
)

type DetailRow struct {
	PrimaryWidth float32
	layout.Inset
}

var DefaultInset = layout.UniformInset(unit.Dp(8))

func (d DetailRow) Layout(gtx layout.Context, primary, detail layout.Widget) layout.Dimensions {
	if d.PrimaryWidth == 0 {
		d.PrimaryWidth = 0.3
	}
	if d.Inset == (layout.Inset{}) {
		d.Inset = DefaultInset
	}
	return layout.Flex{Alignment: layout.Middle}.Layout(gtx,
		layout.Flexed(d.PrimaryWidth, func(gtx layout.Context) layout.Dimensions {
			return d.Inset.Layout(gtx, primary)
		}),
		layout.Flexed(1-d.PrimaryWidth, func(gtx layout.Context) layout.Dimensions {
			return d.Inset.Layout(gtx, detail)
		}))
}
