package main

import (
	"image/color"
	"os"

	"gioui.org/app"
	"gioui.org/layout"
	"gioui.org/op"
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

type appcomponent struct {
	*component.ModalLayer
	*component.AppBar
	*material.Theme
	plusBtn, ov1btn, ctxbtn widget.Clickable
	plusBtnCreating         bool
	whichbtn                map[any]string
}

func loop(w *app.Window) error {
	var ops op.Ops
	modal := component.NewModal()
	appbar := component.NewAppBar(modal)
	heartIcon, _ := widget.NewIcon(icons.ActionFavorite)
	settingIcon, _ := widget.NewIcon(icons.ActionSettingsApplications)
	playIcon, _ := widget.NewIcon(icons.AVPlayArrow)
	appbar.NavigationIcon = settingIcon
	appbar.Title = "Example appbar"
	th := material.NewTheme()

	appstate := appcomponent{
		ModalLayer: modal,
		AppBar:     appbar,
		Theme:      th,
	}
	appstate.whichbtn = make(map[any]string)
	appstate.whichbtn[&appstate.plusBtn] = "plusbtn"
	appstate.whichbtn[&appstate.ov1btn] = "ov1btn"
	appstate.whichbtn[&appstate.ctxbtn] = "ctxbtn"

	appbarActions := []component.AppBarAction{
		{
			OverflowAction: component.OverflowAction{
				Name: "Create",
				Tag:  &appstate.plusBtn,
			},
			Layout: func(gtx layout.Context, bg, fg color.NRGBA) layout.Dimensions {
				if appstate.plusBtn.Clicked(gtx) {
					appstate.plusBtnCreating = !appstate.plusBtnCreating
				}
				ic, _ := widget.NewIcon(icons.ContentCreate)
				btn := component.SimpleIconButton(bg, fg, &appstate.plusBtn, ic)
				btn.Background = bg
				if appstate.plusBtnCreating {
					btn.Color = color.NRGBA{G: 0xff, A: 0xff}
				} else {
					btn.Color = fg
				}
				return btn.Layout(gtx)
			},
		},
		{
			OverflowAction: component.OverflowAction{
				Name: "Contextual", Tag: &appstate.ctxbtn,
			},
			Layout: func(gtx layout.Context, bg, fg color.NRGBA) layout.Dimensions {
				if appstate.ctxbtn.Clicked(gtx) {
					appstate.ToggleContextual(gtx.Now, "Contextual Demo")
				}
				return component.SimpleIconButton(bg, fg, &appstate.ctxbtn, playIcon).Layout(gtx)
			},
		},
	}
	appbarOverflows := []component.OverflowAction{
		{
			Name: "Overflow 1",
			Tag:  &appstate.ov1btn,
		},
	}
	appstate.AppBar.SetActions(appbarActions, appbarOverflows)
	appstate.AppBar.SetContextualActions([]component.AppBarAction{
		component.SimpleIconAction(&appstate.ctxbtn, heartIcon,
			component.OverflowAction{Name: "Fav", Tag: &appstate.ctxbtn}),
	},
		[]component.OverflowAction{})
	for {
		switch e := w.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			gtx := app.NewContext(&ops, e)
			thelayout(gtx, appstate)
			e.Frame(gtx.Ops)
		}
	}
}

func thelayout(gtx layout.Context, appstate appcomponent) layout.Dimensions {
	for _, event := range appstate.AppBar.Events(gtx) {
		switch event := event.(type) {
		case component.AppBarNavigationClicked:
			_ = event
		case component.AppBarContextMenuDismissed:
		case component.AppBarOverflowActionClicked:
			wc, ok := event.Tag.(*widget.Clickable)
			if !ok {
				continue
			}
			if s := appstate.whichbtn[wc]; s == "ctxbtn" {
				appstate.ToggleContextual(gtx.Now, "Contextual Mode")
			} else {
				wc.Click()
			}
		}
	}
	appbarpos := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return appstate.AppBar.Layout(gtx, appstate.Theme, "Menu", "Actions")
	})
	layout.Flex{Axis: layout.Vertical}.Layout(gtx, appbarpos)
	appstate.ModalLayer.Layout(gtx, appstate.Theme)
	return layout.Dimensions{Size: gtx.Constraints.Max}
}
