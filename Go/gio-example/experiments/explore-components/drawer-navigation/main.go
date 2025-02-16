package main

import (
	"image/color"
	"log"
	"os"
	"time"

	"gioui.org/app"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/component"
	"golang.org/x/exp/shiny/materialdesign/icons"
)

func main() {
	go func() {
		window := new(app.Window)
		if err := loop(window); err != nil {
			log.Fatal(err)
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
}

func loop(w *app.Window) error {
	var (
		ops      op.Ops
		modal    = component.NewModal()
		nav      = component.NewNav("Setting Drawer", "This is setting")
		modalNav = component.ModalNavFrom(&nav, modal)
		anim     = component.VisibilityAnimation{
			State:    component.Invisible,
			Duration: time.Millisecond * 250,
		}
	)
	th := material.NewTheme()
	ab := component.NewAppBar(modal)
	settingIcon, _ := widget.NewIcon(icons.ActionSettingsApplications)
	ab.NavigationIcon = settingIcon
	ab.Title = "Test appbar"
	ac := appcomp{
		AppBar:         ab,
		ModalLayer:     modal,
		ModalNavDrawer: modalNav,
		navAnim:        anim,
		NavDrawer:      &nav,
	}
	for {
		switch e := w.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			gtx := app.NewContext(&ops, e)
			thelayout(gtx, th, ac)
			e.Frame(gtx.Ops)
		}
	}
}

func thelayout(gtx layout.Context, th *material.Theme, ac appcomp) layout.Dimensions {
	for _, event := range ac.AppBar.Events(gtx) {
		switch event := event.(type) {
		case component.AppBarNavigationClicked:
			ac.ModalNavDrawer.Appear(gtx.Now)
			ac.navAnim.Disappear(gtx.Now)
		case component.AppBarContextMenuDismissed:
			log.Println("context menu dismissed:", event)
		case component.AppBarOverflowActionClicked:
			log.Println("overflow action selected:", event)
		}
	}
	paint.Fill(gtx.Ops, th.Palette.Bg)
	rigid := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return ac.AppBar.Layout(gtx, th, "Menu", "Actions")
	})
	dummy := layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
		return layout.Flex{}.Layout(gtx,
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				gtx.Constraints.Max.X /= 3
				return ac.NavDrawer.Layout(gtx, th, &ac.navAnim)
			}),
			layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
				defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
				paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
				paint.PaintOp{}.Add(gtx.Ops)
				return layout.Dimensions{Size: gtx.Constraints.Max}
			}),
		)
	})
	layout.Flex{Axis: layout.Vertical}.Layout(gtx, rigid, dummy)
	ac.ModalLayer.Layout(gtx, th)
	return layout.Dimensions{Size: gtx.Constraints.Max}
}
