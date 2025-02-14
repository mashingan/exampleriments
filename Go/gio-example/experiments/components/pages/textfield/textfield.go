package textfield

import (
	"image/color"
	"unicode"

	"gio.test/experiments/components/applayout"
	icon "gio.test/experiments/components/icons"
	"gio.test/experiments/components/pages"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/text"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/component"
)

type Page struct {
	inputAlignment          text.Alignment
	inputAlignmentEnum      widget.Enum
	nameInput, addressInput component.TextField
	priceInput, tweetInput  component.TextField
	numberInput             component.TextField
	widget.List
	*pages.Router
}

func New(r *pages.Router) *Page {
	return &Page{
		Router: r,
	}
}

var _ pages.Pager = &Page{}

func (p *Page) Actions() []component.AppBarAction {
	return []component.AppBarAction{}
}

func (p *Page) Overflow() []component.OverflowAction {
	return []component.OverflowAction{}
}

func (p *Page) NavItem() component.NavItem {
	return component.NavItem{
		Name: "Text Field Features",
		Icon: icon.EditIcon,
	}
}

const (
	hoverInfo   = "Response to hover events"
	labelAninfo = "Label animates properly when you click to select the text field."
	sufprefInfo = "Can have suffix and prefix element."
	attribution = "This text field implementation was contributed by Jack Mordaunt. Thanks Jack!"
)

func (p *Page) Layout(gtx layout.Context, th *material.Theme) layout.Dimensions {
	p.List.Axis = layout.Vertical
	priceInput := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		p.priceInput.Prefix = func(gtx layout.Context) layout.Dimensions {
			th := *th
			th.Palette.Fg = color.NRGBA{R: 100, B: 100, G: 100, A: 255}
			return material.Label(&th, th.TextSize, "$").Layout(gtx)
		}
		p.priceInput.Suffix = func(gtx layout.Context) layout.Dimensions {
			th := *th
			th.Palette.Fg = color.NRGBA{R: 100, B: 100, G: 100, A: 255}
			return material.Label(&th, th.TextSize, "00").Layout(gtx)
		}
		p.priceInput.SingleLine = true
		p.priceInput.Alignment = p.inputAlignment
		return p.priceInput.Layout(gtx, th, "Price")
	})
	digitOnly := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		if err := func() string {
			for _, r := range p.numberInput.Text() {
				if !unicode.IsDigit(r) {
					return "Must contain digit only"
				}
			}
			return ""
		}(); err != "" {
			p.numberInput.SetError(err)
		} else {
			p.numberInput.ClearError()
		}
		p.numberInput.SingleLine = true
		p.numberInput.Alignment = p.inputAlignment
		return p.numberInput.Layout(gtx, th, "Number")
	})
	tweetInput := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		if p.tweetInput.TextTooLong() {
			p.tweetInput.SetError("Too many characters")
		} else {
			p.tweetInput.ClearError()
		}
		p.tweetInput.CharLimit = 128
		p.tweetInput.Helper = "Tweets have limited character count"
		p.tweetInput.Alignment = p.inputAlignment
		return p.tweetInput.Layout(gtx, th, "Tweet")
	})
	radios := make([]layout.FlexChild, 3)
	type _rer struct{ a, b string }
	for i, e := range []_rer{
		{layout.Start.String(), "Start"},
		{layout.Middle.String(), "Middle"},
		{layout.End.String(), "End"}} {
		radios[i] = layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return material.RadioButton(th, &p.inputAlignmentEnum,
				e.a, e.b).Layout(gtx)
		})
	}
	guardingInput := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		if p.inputAlignmentEnum.Update(gtx) {
			switch p.inputAlignmentEnum.Value {
			case layout.Start.String():
				p.inputAlignment = text.Start
			case layout.Middle.String():
				p.inputAlignment = text.Middle
			case layout.End.String():
				p.inputAlignment = text.End
			default:
				p.inputAlignment = text.Start
			}
			gtx.Execute(op.InvalidateCmd{})
		}
		return applayout.DefaultInset.Layout(gtx,
			func(gtx layout.Context) layout.Dimensions {
				return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
					layout.Rigid(func(gtx layout.Context) layout.Dimensions {
						return material.Body2(th, "Text alignment").Layout(gtx)
					}),
					layout.Rigid(func(gtx layout.Context) layout.Dimensions {
						return layout.Flex{Axis: layout.Vertical}.Layout(gtx, radios...)
					}))
			},
		)
	})
	return material.List(th, &p.List).Layout(gtx, 1,
		func(gtx layout.Context, index int) layout.Dimensions {
			return layout.Flex{
				Axis: layout.Vertical,
			}.Layout(gtx,
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					p.nameInput.Alignment = p.inputAlignment
					return p.nameInput.Layout(gtx, th, "Name")
				}),
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return applayout.DefaultInset.Layout(gtx,
						material.Body2(th, hoverInfo).Layout)
				}),
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					p.addressInput.Alignment = p.inputAlignment
					return p.addressInput.Layout(gtx, th, "Address")
				}),
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return applayout.DefaultInset.Layout(gtx,
						material.Body2(th, labelAninfo).Layout)
				}),
				priceInput,
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return applayout.DefaultInset.Layout(gtx,
						material.Body2(th, sufprefInfo).Layout)
				}),
				digitOnly,
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return applayout.DefaultInset.Layout(gtx,
						material.Body2(th, "Can be validated").Layout)
				}),
				tweetInput,
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return applayout.DefaultInset.Layout(gtx,
						material.Body2(th, "Can have character count and help text").Layout)
				}),
				guardingInput,
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return applayout.DefaultInset.Layout(gtx,
						material.Body2(th, attribution).Layout)
				}),
			)
		})
}
