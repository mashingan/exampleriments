package about

import (
	"io"
	"strings"

	"gio.test/experiments/components/applayout"
	icon "gio.test/experiments/components/icons"
	"gio.test/experiments/components/pages"
	"gioui.org/io/clipboard"
	"gioui.org/layout"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/component"
)

type Page struct {
	eliasCpBtn, ChrisCpBtnGh, chrisCpBtnLp widget.Clickable
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
		Name: "About this library",
		Icon: icon.OtherIcon,
	}
}

const (
	sponsorElias   = "https://github.com/sponsors/eliasnaur"
	sponsorChrisGh = "https://github.com/sponsors/whereswaldon"
	sponsorChrisLp = "https://liberapay.com/whereswaldon"
	sponsorNotice  = "This library implements material design components from https://material.io" +
		" using https://gioui.org.\n" +
		"If you like this library work with it, please consider sponsoring Elias and/or Chris!"
)

func (p *Page) Layout(gtx layout.Context, th *material.Theme) layout.Dimensions {
	p.List.Axis = layout.Vertical
	return material.List(th, &p.List).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
		return layout.Flex{
			Alignment: layout.Middle,
			Axis:      layout.Vertical,
		}.Layout(gtx,
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				return applayout.DefaultInset.Layout(gtx,
					material.Body1(th, sponsorNotice).Layout)
			}),
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				return applayout.DetailRow{}.Layout(gtx,
					material.Body1(th, "Elias Naur can be sponsored  on Github at "+sponsorElias).Layout, func(gtx layout.Context) layout.Dimensions {
						if p.eliasCpBtn.Clicked(gtx) {
							gtx.Execute(clipboard.WriteCmd{
								Data: io.NopCloser(strings.NewReader(sponsorElias)),
							})
						}
						return material.Button(th, &p.eliasCpBtn, "Copy sponsorship URL").Layout(gtx)
					})
			}),
		)
	})
}
