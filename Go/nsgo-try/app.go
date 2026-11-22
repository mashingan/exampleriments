// Command nsgo-try is an NSGo app.
package main

import (
	"fmt"
	"log/slog"
	"strings"
	"time"

	"github.com/nsgo-dev/nsgo"
	"github.com/nsgo-dev/nsgo/base"
	"github.com/nsgo-dev/nsgo/core"
	"github.com/nsgo-dev/nsgo/icons/lucide"
	"github.com/nsgo-dev/nsgo/mobile"
	"github.com/nsgo-dev/nsgo/ui"
)

func mainPage(cx *core.Scope, nav *base.Nav[string], urlsetting *core.Signal[string]) core.Node {
	colors := ui.ColorsOf(cx)
	draft := core.NewSignal(cx, "")
	showtxt := core.NewSignal(cx, "")
	fade := core.NewAnimatedValue(cx, 0)
	showDraft := func() {
		txt := strings.TrimSpace(draft.Get())
		if txt == "" {
			return
		}
		showtxt.Set(txt)
		draft.Set("")
		dur := 500 * time.Millisecond
		cfg := core.TimingConfig{
			Duration: dur,
			Easing:   core.Easing{Mode: core.EaseModeOut},
		}
		cx.StartAnimation(core.Timing(fade, 1, cfg), nil)
		cx.Post(func() {
			<-time.After(dur)
			cx.StartAnimation(core.Timing(fade, 0, cfg), nil)
		})
	}
	adders := []core.Node{
		ui.Input{
			Style:       core.Style{FlexGrow: 1},
			Placeholder: "What is the plan?",
			OnChange:    draft.Set,
			Value:       draft.Get,
			OnSubmit:    func(string) { showDraft() },
		},
		core.View{
			Style: core.Style{Width: core.Pct(20)},
			Children: []core.Node{
				ui.Button{Text: "Show", Icon: lucide.Antenna, OnPress: showDraft, TestID: "showbtn"},
			},
		},
	}
	addrow := core.View{
		Style:    core.Style{Direction: core.Row, Gap: 16},
		Children: adders,
	}
	showNotif := func() {
		cx.RequestNotificationPermission(func(p core.NotificationPermission) {
			if p == core.NotificationsGranted || p == core.NotificationsUnavailable {
				cx.ShowNotification(core.Notification{ID: "chat:42", Title: showtxt.Get(), Body: "Launch?"}, nil)
			} else {
				slog.Info("apparent no notifications granted:", "permission", p)
			}
		})
	}
	infoshow := core.View{
		Style: core.Style{
			Animated: &core.AnimatedStyle{Opacity: fade},
			FlexGrow: 1,
			// MinWidth: core.Pt(20),
		},
		Children: []core.Node{
			ui.Textf(showtxt.Get, 14, ui.Semibold, func() core.Color {
				return core.RGB(0xff0099)
			}),
		},
	}
	mobile.PlayHaptic(cx, mobile.HapticSelection)

	sizeSelect := core.NewSignal(cx, "")
	selectWidgets := ui.Select[string]{
		Options: []ui.Option[string]{
			{Value: "s", Label: "Small"},
			{Value: "m", Label: "Medium"},
			{Value: "l", Label: "Large"},
			{Value: "xl", Label: "Extra Large"},
		},
		Value:       sizeSelect.Get,
		OnChange:    sizeSelect.Set,
		Label:       "Size",
		Placeholder: "Select size",
		Style:       core.Style{Width: core.Pct(40)},
	}
	dlgt := core.NewSignal(cx, false)
	dlg := ui.Dialog{
		Open: dlgt.Get,
		OnOpenChange: func(open bool, reason base.Reason) {
			dlgt.Set(true)
		},
		OnClose: func() {
		},
		Title:       "Rename",
		Description: "The new name shows everywhere.",
		Children: []core.Node{
			core.View{
				Style: core.Style{
					Padding:    core.All(16),
					PaddingEnv: core.EnvEdges{Bottom: core.EnvKeyboard},
				},
				Children: []core.Node{ui.Input{Value: draft.Get, OnChange: draft.Set}},
			},
		},
		Footer: []core.Node{
			ui.Button{Text: "Cancel", Variant: ui.ButtonSecondary, OnPress: func() { dlgt.Set(false); draft.Set("") }},
			ui.Button{Text: "Save", OnPress: func() { dlgt.Set(false); showDraft() }},
		},
		NoCloseOnEscape:        false,
		NoCloseOnBackdropPress: false,
		Inset:                  0,
		Bare:                   false,
		Label:                  "",
		Role:                   0,
		TestID:                 "",
		Style: core.Style{
			Padding:    core.Symmetric(5, 20),
			PaddingEnv: core.EnvAll(core.EnvNone),
			Gap:        5,
		},
	}
	fromrgb := func(r, g, b uint) uint32 {
		h := uint32(0)
		h |= (uint32(r) << 8)
		h |= (uint32(g) << 16)
		h |= (uint32(b) << 24)
		return h
	}
	todos := core.View{Style: core.Style{Direction: core.Row}}
	txts := []core.Span{}
	for _, c := range "todos" {
		cl := uint32(1 * 255 / 26)
		var r, g, b uint
		switch c % 2 {
		case 0:
			r, g, b = uint(255-cl), 255, 255
		case 1:
			r, g, b = 255, uint(255-cl), 255
		case 2:
			r, g, b = 255, 255, uint(255-cl)
		}
		txts = append(txts, core.Span{
			Content: fmt.Sprintf("%c", c),
			Style: core.Style{
				FontSize: 28, FontWeight: ui.Bold,
				Background: core.Color(fromrgb(r, g, b)),
			},
		})
	}
	todos.Children = append(todos.Children, core.Text{Spans: txts})
	floathidden := core.NewSignal(cx, false)
	floatdur := 500 * time.Millisecond
	floating := core.View{
		StyleFunc: func() core.Style {
			display := core.DisplayNone
			if floathidden.Get() {
				display = core.DisplayFlex
				cx.Post(func() {
					<-time.After(3 * floatdur)
					floathidden.Set(false)
				})
			}
			return core.Style{
				// Animated: &core.AnimatedStyle{Opacity: fade},
				Position: core.Absolute,
				Top:      core.Pct(5),
				Right:    core.Pct(5),
				ZIndex:   99,
				Display:  display,
			}
		},
		Children: []core.Node{
			ui.IconButton{
				Icon:    lucide.Galaxy,
				Variant: ui.IconButtonRaised,
				OnPress: func() {
					slog.Debug("IconFloating:OnPress")
					nav.Push("Setting")
				},
				Tint: func() core.Color {
					return core.RGB(0xfe3211)
				},
			},
		},
	}
	settinginfo := ui.Textf(urlsetting.Get, 14, ui.Regular, colors.Text)
	prevTouch := time.Time{}
	return core.View{
		Style: core.Style{
			FlexGrow: 1, FlexShrink: 1, Padding: core.All(24), PaddingEnv: core.EnvAll(core.EnvSafeArea), Gap: 16,
			// Background: core.RGB(0xffffff),
		},
		OnPress: func(p core.PressEvent) {
			// slog.Info("OnPress main", "onpress-clicks", p.Clicks)
			if p.Clicks == 2 {
				if cx.Env().OS() == "android" {
					slog.Info("Double click in Android")
				}
				floathidden.Set(true)
			}
		},
		OnTouch: func(te core.TouchEvent) {
			switch te.Phase {
			case core.TouchStart:
				return
			case core.TouchEnd:
				if prevTouch.After(time.Time{}) && time.Since(prevTouch) <= 500*time.Millisecond {
					floathidden.Set(true)
					return
				}
				prevTouch = time.Now()
				return
			}
		},
		Children: []core.Node{
			floating,
			// todos,
			ui.Text("Todos", 28, ui.Bold, func() core.Color {
				return core.Color(core.GradientLinear)
			}),
			addrow,
			core.View{
				Style: core.Style{Direction: core.RowReverse, FlexShrink: 1, Gap: 8},
				Children: []core.Node{
					core.View{Style: core.Style{Width: core.Pct(20)},
						Children: []core.Node{
							ui.Button{Text: "Notif?", Icon: lucide.Rocket, OnPress: showNotif, TestID: "notifbtn"},
						},
					},
					// core.View{Style: core.Style{FlexShrink: 0}},
					infoshow,
				},
			},
			selectWidgets,
			dlg,
			core.View{
				Style: core.Style{Width: core.Pct(40)},
				Children: []core.Node{
					ui.Button{Text: "Open Dialog", OnPress: func() { dlgt.Set(true) }},
				},
			},
			core.View{
				Style: core.Style{Direction: core.Row, Gap: 10},
				Children: []core.Node{
					ui.Text("Setting:", 14, ui.Semibold, colors.Text),
					settinginfo,
					core.View{
						StyleFunc: func() core.Style {
							display := core.DisplayNone
							if urlsetting.Get() != "" {
								display = core.DisplayFlex
							}
							return core.Style{Display: display}
						},
						Children: []core.Node{
							ui.IconButton{Icon: lucide.Copy, Variant: ui.IconButtonGhost, OnPress: func() {
								cx.SetClipboard(urlsetting.Get())
							}},
						},
					},
				},
			},
			ui.Spacer{},
		},
	}
}

func settingInfo(cx *core.Scope, nav *base.Nav[string], url *core.Signal[string]) base.NavPage {
	return ui.NavPage(cx, base.NavPage{
		Title: "Setting",
		Children: []core.Node{
			core.View{
				Style: core.Style{Direction: core.Row, Gap: 16, Padding: core.All(10), Height: core.Pct(10)},
				Children: []core.Node{
					ui.Input{
						Style:       core.Style{FlexGrow: 1},
						Placeholder: "Set the url target?",
						OnChange:    url.Set,
						Value:       url.Get,
						OnSubmit:    func(string) { nav.Pop(1) },
					},
					core.View{
						Style: core.Style{Width: core.Pct(20)},
						Children: []core.Node{
							ui.Button{Text: "Set", Icon: lucide.Plus, OnPress: func() { nav.Pop(1) }, TestID: "setbtn"},
						},
					},
				},
			},
		},
	}, ui.NavHeader{
		Trailing: []core.Node{switchTheme(cx)},
	})

}

func Root(cx *core.Scope) core.Node {
	nav := base.NewNav(cx, "Main", "Front")
	urlsetting := core.NewSignal(cx, "")
	env := cx.Env()
	return nav.Stack(core.Style{FlexGrow: 1}, func(cx *core.Scope, route string, depth func() int) base.NavPage {
		slog.Debug("Root:stack", "route", route)
		switch route {
		case "Main":
			return ui.NavPage(cx, base.NavPage{
				Title: "Main Page",
				Children: []core.Node{
					mainPage(cx, nav, urlsetting),
				},
			}, ui.NavHeader{
				Trailing: []core.Node{
					switchTheme(cx),
					ui.IconButton{Icon: lucide.House, Label: "Front",
						Variant: ui.IconButtonRaised, OnPress: func() { nav.Push("Front") }},
				},
			})
		case "Setting":
			return settingInfo(cx, nav, urlsetting)
		}
		colors := ui.ColorsOf(cx)
		infos := []core.Node{}
		cx.SetAppearance(core.AppearanceLight)
		for _, s := range []string{
			fmt.Sprintf("Is desktop: %t", env.Desktop()),
			fmt.Sprintf("Device name: %s", env.DeviceName()),
			fmt.Sprintf("Device model: %s", env.DeviceModel()),
			fmt.Sprintf("Locale: %s", env.Locale()),
			fmt.Sprintf("OS: %s", env.OS()),
			fmt.Sprintf("OS version: %s", env.OSVersion()),
			fmt.Sprintf("Is dark theme? %t", env.Dark()),
		} {
			infos = append(infos, ui.Text(s, 14, ui.Regular, colors.Text))
		}
		return ui.NavPage(cx, base.NavPage{
			Title:       "Front base",
			OnBackPress: func() { nav.Pop(1) },
			Children: []core.Node{
				core.View{
					Style:    core.Style{Margin: core.All(10)},
					Children: infos,
				},
			},
		}, ui.NavHeader{
			OnBack:    func() { nav.Pop(1) },
			BackLabel: "Main",
			Trailing: []core.Node{
				// ui.IconButton{Icon: lucide.Archive, Label: "Archive",
				// 	Variant: ui.IconButtonGhost, OnPress: func() {}},
				switchTheme(cx),
			},
		})
	})
}

func switchTheme(cx *core.Scope) core.Node {
	return ui.IconButton{Icon: lucide.SunMoon, Label: "Switch-Theme",
		Variant: ui.IconButtonGhost, OnPress: func() {

			appearance := core.AppearanceDark
			if cx.Dark() {
				appearance = core.AppearanceLight
			}
			cx.SetAppearance(appearance)
		},
	}
}

func main() { nsgo.Run(Root) }
