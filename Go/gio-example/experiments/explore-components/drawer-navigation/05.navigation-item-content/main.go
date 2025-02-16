package main

import (
	"image/color"
	"os"
	"time"

	"gioui.org/app"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/unit"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/component"
	"golang.org/x/exp/shiny/materialdesign/icons"
)

func main() {
	go func() {
		window := new(app.Window)
		if err := loop(window); err != nil {
			panic(err)
		}
		os.Exit(0)
	}()
	app.Main()
}

type appcomp struct {
	*component.ModalLayer
	*component.AppBar
	*component.ModalNavDrawer
	*component.NavDrawer
	navAnim component.VisibilityAnimation
	page    int
}

func loop(w *app.Window) error {
	var (
		ops      op.Ops
		modal    = component.NewModal()
		nav      = component.NewNav("Navigation Drawer", "This is navigation")
		modalNav = component.ModalNavFrom(&nav, modal)
		th       = material.NewTheme()
		ab       = component.NewAppBar(modal)
		anim     = component.VisibilityAnimation{
			State:    component.Invisible,
			Duration: time.Millisecond * 250,
		}
	)
	navIcon, _ := widget.NewIcon(icons.NavigationApps)
	ab.NavigationIcon = navIcon
	ab.Title = "Nav"
	ac := appcomp{
		AppBar:         ab,
		ModalLayer:     modal,
		ModalNavDrawer: modalNav,
		NavDrawer:      &nav,
		navAnim:        anim,
	}
	nav.AddNavItem(component.NavItem{Tag: 0, Name: "Nav"})
	nav.AddNavItem(component.NavItem{Tag: 1, Name: "Page 1"})
	ac.page = 0
	for {
		switch e := w.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			gtx := app.NewContext(&ops, e)
			thelayout(gtx, th, &ac)
			e.Frame(gtx.Ops)
		}
	}
}

func thelayout(gtx layout.Context, th *material.Theme, ac *appcomp) layout.Dimensions {
	for _, event := range ac.AppBar.Events(gtx) {
		switch event := event.(type) {
		case component.AppBarNavigationClicked:
			ac.ModalNavDrawer.Appear(gtx.Now)
			ac.navAnim.Disappear(gtx.Now)
		case component.AppBarContextMenuDismissed:
			_ = event
		case component.AppBarOverflowActionClicked:
			//
		}
	}
	if ac.ModalNavDrawer.NavDestinationChanged() {
		ac.page = ac.ModalNavDrawer.CurrentNavDestination().(int)
		if ac.page == 0 {
			ac.AppBar.Title = "Nav"
		} else if ac.page == 1 {
			ac.AppBar.Title = "Page 1"
		}
	}
	paint.Fill(gtx.Ops, th.Palette.Bg)
	abar := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return ac.AppBar.Layout(gtx, th, "Menu", "Actions")
	})
	drawer := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		gtx.Constraints.Max.X /= 3
		return ac.NavDrawer.Layout(gtx, th, &ac.navAnim)
	})
	var content layout.FlexChild
	if ac.page == 0 {
		content = layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{}.Layout(gtx,
				drawer,
				layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
					defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
					paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
					paint.PaintOp{}.Add(gtx.Ops)
					return layout.Dimensions{Size: gtx.Constraints.Max}

				}))
		})

	} else if ac.page == 1 {
		content = layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{}.Layout(gtx, drawer,
				layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
					return layout.UniformInset(unit.Dp(8)).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						return material.H2(th, "Content page 1").Layout(gtx)
					})
				}))
		})

	}
	layout.Flex{Axis: layout.Vertical}.Layout(gtx, abar, content)
	ac.ModalLayer.Layout(gtx, th)
	return layout.Dimensions{Size: gtx.Constraints.Max}
}
