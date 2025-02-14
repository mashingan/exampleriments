package menu

import (
	"fmt"
	"image"
	"image/color"

	icon "gio.test/experiments/components/icons"
	"gio.test/experiments/components/pages"
	"gioui.org/layout"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/unit"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/component"
)

type Page struct {
	redBtn, greenBtn, blueBtn       widget.Clickable
	balanceBtn, accountBtn, cartBtn widget.Clickable
	leftFillColor                   color.NRGBA
	leftContextArea                 component.ContextArea
	leftMenu, rightMenu             component.MenuState
	menuInit                        bool
	menuDemoList                    widget.List
	menuDemoListStates              []component.ContextArea
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
		Name: "Menu Features",
		Icon: icon.RestaurantMenuIcon,
	}
}

const (
	menuInfo = "Menus support arbitrary widgets.\nThis is just label!.\n" +
		"Here's a loader:"
)

func (p *Page) setMenu(th *material.Theme) {
	p.leftMenu = component.MenuState{
		Options: []func(layout.Context) layout.Dimensions{
			func(gtx layout.Context) layout.Dimensions {
				return layout.Inset{
					Left:  unit.Dp(16),
					Right: unit.Dp(16),
				}.Layout(gtx, material.Body1(th, menuInfo).Layout)
			},

			component.Divider(th).Layout,
			func(gtx layout.Context) layout.Dimensions {
				return layout.Inset{
					Top:    unit.Dp(4),
					Bottom: unit.Dp(4),
					Left:   unit.Dp(16),
					Right:  unit.Dp(16),
				}.Layout(gtx,
					func(gtx layout.Context) layout.Dimensions {
						gtx.Constraints.Max.X = gtx.Dp(unit.Dp(24))
						gtx.Constraints.Max.Y = gtx.Dp(unit.Dp(24))
						return material.Loader(th).Layout(gtx)
					})
			},
			component.SubheadingDivider(th, "Colors").Layout,
			component.MenuItem(th, &p.redBtn, "Red").Layout,
			component.MenuItem(th, &p.greenBtn, "Green").Layout,
			component.MenuItem(th, &p.blueBtn, "Blue").Layout,
		},
	}
	p.rightMenu = component.MenuState{
		Options: []func(layout.Context) layout.Dimensions{
			func(gtx layout.Context) layout.Dimensions {
				item := component.MenuItem(th, &p.balanceBtn, "Balance")
				item.Icon = icon.AccountBalanceIcon
				item.Hint = component.MenuHintText(th, "Hint")
				return item.Layout(gtx)
			},

			func(gtx layout.Context) layout.Dimensions {
				item := component.MenuItem(th, &p.accountBtn, "Account")
				item.Icon = icon.AccountBoxIcon
				item.Hint = component.MenuHintText(th, "Hint")
				return item.Layout(gtx)
			},
			func(gtx layout.Context) layout.Dimensions {
				item := component.MenuItem(th, &p.cartBtn, "Cart")
				item.Icon = icon.CartIcon
				item.Hint = component.MenuHintText(th, "Hint")
				return item.Layout(gtx)
			},
		},
	}
}

func (p *Page) Layout(gtx layout.Context, th *material.Theme) layout.Dimensions {
	if !p.menuInit {
		p.setMenu(th)
	}
	if p.redBtn.Clicked(gtx) {
		p.leftFillColor = color.NRGBA{R: 200, A: 255}
	}
	if p.greenBtn.Clicked(gtx) {
		p.leftFillColor = color.NRGBA{G: 200, A: 255}
	}
	if p.blueBtn.Clicked(gtx) {
		p.leftFillColor = color.NRGBA{B: 200, A: 255}
	}
	menusArea := make([]layout.FlexChild, 2)
	menusArea[0] = layout.Flexed(0.5, func(gtx layout.Context) layout.Dimensions {
		return widget.Border{
			Color: color.NRGBA{A: 255},
			Width: unit.Dp(2),
		}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			s1 := func(gtx layout.Context) layout.Dimensions {
				mx := image.Pt(gtx.Constraints.Max.X, gtx.Constraints.Max.Y)
				rect := image.Rectangle{Max: mx}
				paint.FillShape(gtx.Ops, p.leftFillColor, clip.Rect(rect).Op())
				return layout.Dimensions{Size: mx}
			}
			s2Region := func(gtx layout.Context) layout.Dimensions {
				return layout.UniformInset(unit.Dp(12)).Layout(gtx,
					material.Body1(th, "Right click in this region").Layout)
			}
			s2Surface := func(gtx layout.Context) layout.Dimensions {
				return component.Surface(th).Layout(gtx, s2Region)
			}
			s2 := func(gtx layout.Context) layout.Dimensions {
				return layout.UniformInset(unit.Dp(12)).Layout(gtx, s2Surface)
			}
			expCtrArea := func(gtx layout.Context) layout.Dimensions {
				gtx.Constraints.Min = image.Point{}
				return component.Menu(th, &p.leftMenu).Layout(gtx)
			}
			return layout.Stack{}.Layout(gtx,
				layout.Stacked(s1),
				layout.Stacked(s2),
				layout.Expanded(func(gtx layout.Context) layout.Dimensions {
					return p.leftContextArea.Layout(gtx, expCtrArea)
				}),
			)
		})
	})
	menusArea[1] = layout.Flexed(0.5, func(gtx layout.Context) layout.Dimensions {
		p.menuDemoList.Axis = layout.Vertical
		return material.List(th, &p.menuDemoList).Layout(gtx, 30, func(gtx layout.Context, index int) layout.Dimensions {
			if len(p.menuDemoListStates) < index+1 {
				p.menuDemoListStates = append(p.menuDemoListStates, component.ContextArea{})
			}
			state := &p.menuDemoListStates[index]
			s1 := func(gtx layout.Context) layout.Dimensions {
				gtx.Constraints.Min.X = gtx.Constraints.Max.X
				return layout.UniformInset(unit.Dp(8)).Layout(gtx,
					material.Body1(th, fmt.Sprintf("Item %d", index)).Layout)
			}
			exp := func(gtx layout.Context) layout.Dimensions {
				return state.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					gtx.Constraints.Min.X = 0
					return component.Menu(th, &p.rightMenu).Layout(gtx)
				})
			}
			return layout.Stack{}.Layout(gtx,
				layout.Stacked(s1),
				layout.Expanded(exp),
			)
		})
	})
	return layout.Flex{}.Layout(gtx, menusArea...)
}
