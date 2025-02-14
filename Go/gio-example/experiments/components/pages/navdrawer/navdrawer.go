package navdrawer

import (
	"gio.test/experiments/components/applayout"
	icon "gio.test/experiments/components/icons"
	"gio.test/experiments/components/pages"
	"gioui.org/layout"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/component"
)

type Page struct {
	NonModalDrawer widget.Bool
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
		Name: "Nav Drawer Features",
		Icon: icon.SettingsIcon,
	}
}

func (p *Page) detailNonModal(th *material.Theme) layout.Widget {
	return func(gtx layout.Context) layout.Dimensions {
		if p.NonModalDrawer.Update(gtx) {
			p.Router.NonModalDrawer = p.NonModalDrawer.Value
			if p.NonModalDrawer.Value {
				p.Router.NavAnim.Appear(gtx.Now)
			} else {
				p.Router.NavAnim.Disappear(gtx.Now)
			}
		}
		return material.Switch(th, &p.NonModalDrawer, "Use non-modal navigation drawer").Layout(gtx)
	}
}

const (
	body1Label = "The nav bar widget provides a consistent interface element" +
		" for navigation.\n" +
		"The control belows allow you to see the various features available in our Navigation Drawer implementation."
	dragInfo         = "You can close modal nav drawer by dragging it to left"
	touchScrimInfo   = "You can close the modal nav drawer touching anywhere in translucent scrim to the right"
	bottomAnchorInfo = "If you toggle support for the bottom app bar in App Bar settings, " +
		"nav drawer content will anchor to the bottom of the drawer area instead of the top."
)

func (p *Page) Layout(gtx layout.Context, th *material.Theme) layout.Dimensions {
	p.List.Axis = layout.Vertical
	information := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DefaultInset.Layout(gtx, material.Body1(th, body1Label).Layout)
	})
	nonModalDrawer := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DetailRow{}.Layout(gtx,
			material.Body1(th, "Use non modal drawer").Layout,
			p.detailNonModal(th))
	})
	dragClose := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DetailRow{}.Layout(gtx,
			material.Body1(th, "Drag to close").Layout,
			material.Body2(th, dragInfo).Layout)
	})
	touchClose := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DetailRow{}.Layout(gtx,
			material.Body1(th, "Touch scrim to close").Layout,
			material.Body2(th, touchScrimInfo).Layout)
	})
	bottomAnchor := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return applayout.DetailRow{}.Layout(gtx,
			material.Body1(th, "Bottom content anchoring").Layout,
			material.Body2(th, bottomAnchorInfo).Layout)
	})
	return material.List(th, &p.List).Layout(gtx, 1,
		func(gtx layout.Context, index int) layout.Dimensions {
			return layout.Flex{
				Alignment: layout.Middle,
				Axis:      layout.Vertical,
			}.Layout(gtx,
				information,
				nonModalDrawer,
				dragClose,
				touchClose,
				bottomAnchor)
		})
}
