package appbar

import (
	"image/color"

	"gio.test/experiments/components/applayout"
	icon "gio.test/experiments/components/icons"
	"gio.test/experiments/components/pages"
	"gioui.org/layout"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/component"
)

type Page struct {
	heartBtn, plusBtn, contextBtn          widget.Clickable
	exampleOverflowState, red, green, blue widget.Clickable
	bottomBar, customNavIcon               widget.Bool
	favorited                              bool
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
	return []component.AppBarAction{
		{
			OverflowAction: component.OverflowAction{
				Name: "Favorite",
				Tag:  &p.heartBtn,
			},
			Layout: func(gtx layout.Context, bg, fg color.NRGBA) layout.Dimensions {
				if p.heartBtn.Clicked(gtx) {
					p.favorited = !p.favorited
				}
				btn := component.SimpleIconButton(bg, fg, &p.heartBtn, icon.HeartIcon)
				btn.Background = bg
				if p.favorited {
					btn.Color = color.NRGBA{R: 200, A: 255}
				} else {
					btn.Color = fg
				}
				return btn.Layout(gtx)
			},
		},
		component.SimpleIconAction(&p.plusBtn, icon.PlusIcon,
			component.OverflowAction{
				Name: "Create",
				Tag:  &p.plusBtn,
			}),
	}
}

func (p *Page) Overflow() []component.OverflowAction {
	return []component.OverflowAction{
		{Name: "Example 1", Tag: &p.exampleOverflowState},
		{Name: "Example 2", Tag: &p.exampleOverflowState},
	}
}

func (p *Page) NavItem() component.NavItem {
	return component.NavItem{
		Name: "App Bar Features",
		Icon: icon.HomeIcon,
	}
}

const (
	settingNameColumnWidth   = .3
	settingDetailColumnWidth = 1 - settingNameColumnWidth
	body1Label               = "The app bar widget provides a consistent interface element" +
		" for triggering navigation specific action.\n" +
		"The control belows allow you to see the various features available in our App Bar implementation."
	resizeLabel = "Resize the width of your screen to see app bar actions" +
		" collapse into or emerge from the overflow menu (as size permits)."
	customActionLabel = "Click the heart action to see custom button behaviour"
)

func (p *Page) contextAppBar(th *material.Theme) layout.Widget {
	actions := []component.AppBarAction{
		component.SimpleIconAction(&p.red, icon.HeartIcon,
			component.OverflowAction{Name: "House", Tag: &p.red}),
	}
	return func(gtx layout.Context) layout.Dimensions {
		if p.contextBtn.Clicked(gtx) {
			p.Router.AppBar.SetContextualActions(actions,
				[]component.OverflowAction{
					{Name: "foo", Tag: &p.blue},
					{Name: "bar", Tag: &p.green},
				})
			p.Router.AppBar.ToggleContextual(gtx.Now, "Contextual Title")
		}
		return material.Button(th, &p.contextBtn, "Trigger").Layout(gtx)
	}
}

func (p *Page) customIconBar(th *material.Theme) layout.Widget {
	return func(gtx layout.Context) layout.Dimensions {
		if p.customNavIcon.Update(gtx) {
			if p.customNavIcon.Value {
				p.Router.AppBar.NavigationIcon = icon.HomeIcon
			} else {
				p.Router.AppBar.NavigationIcon = icon.MenuIcon
			}
		}
		return material.Switch(th, &p.customNavIcon, "Use Custom Navigation Icon").Layout(gtx)
	}
}

func (p *Page) whetherBottomBar(th *material.Theme) layout.Widget {
	return func(gtx layout.Context) layout.Dimensions {
		if p.bottomBar.Update(gtx) {
			if p.bottomBar.Value {
				p.Router.ModalNavDrawer.Anchor = component.Bottom
				p.Router.AppBar.Anchor = component.Bottom
			} else {
				p.Router.ModalNavDrawer.Anchor = component.Top
				p.Router.AppBar.Anchor = component.Top
			}
			p.Router.BottomBar = p.bottomBar.Value
		}
		return material.Switch(th, &p.bottomBar, "Use Buttom App Bar").Layout(gtx)
	}
}

func (p *Page) Layout(gtx layout.Context, th *material.Theme) layout.Dimensions {
	p.List.Axis = layout.Vertical
	information := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DefaultInset.Layout(gtx, material.Body1(th, body1Label).Layout)
	})
	contextualAppBar := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DetailRow{}.Layout(gtx,
			material.Body1(th, "Contextual App bar").Layout,
			p.contextAppBar(th))
	})
	customNavIcon := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DetailRow{}.Layout(gtx,
			material.Body1(th, "Custom Navigation Icon").Layout,
			p.customIconBar(th),
		)
	})
	isBottomBar := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DetailRow{}.Layout(gtx,
			material.Body1(th, "Bottom App Bar").Layout,
			p.whetherBottomBar(th),
		)
	})
	resizeInfo := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DetailRow{}.Layout(gtx,
			material.Body1(th, "Animated Resize").Layout,
			material.Body2(th, resizeLabel).Layout)
	})
	cab := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DetailRow{}.Layout(gtx,
			material.Body1(th, "Custom Action Buttons").Layout,
			material.Body2(th, customActionLabel).Layout)
	})
	return material.List(th, &p.List).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
		return layout.Flex{
			Alignment: layout.Middle,
			Axis:      layout.Vertical,
		}.Layout(gtx,
			information,
			contextualAppBar,
			customNavIcon,
			isBottomBar,
			resizeInfo,
			cab,
		)
	})
}
