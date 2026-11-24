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
	demoToast := selectToastType(cx)
	// link2visit := core.NewSignal(cx, "")
	return core.View{
		Style: core.Style{
			FlexGrow: 1, FlexShrink: 1, Padding: core.All(24), PaddingEnv: core.EnvAll(core.EnvSafeArea), Gap: 16,
			OverflowX: core.OverflowScroll,
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
		OnTouch: func() func(core.TouchEvent) {
			prevTouch := time.Time{}
			return func(te core.TouchEvent) {
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
			}
		}(),
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
			demoToast[0],
			demoToast[1],
			ui.Link{
				Text: "https://example.com",
				Href: "https://example.com",
			},
			todoList(cx),
			ui.Link{
				Text: "Go to TodoList 2",
				OnPress: func() {
					nav.Push("TodoList2")
				},
			},
			// ui.Spacer{},
		},
	}
}

func selectToastType(cx *core.Scope) []core.Node {
	toasttype := core.NewSignal(cx, uint8(0))
	toast := ui.NewToaster(cx)
	return []core.Node{
		core.View{
			Style: core.Style{Direction: core.Row, Gap: 8},
			Children: []core.Node{
				ui.Button{
					Text:   "Show Toast",
					TestID: "show-main-toast",
					Style:  core.Style{Width: core.Pct(40)},
					OnPress: func() {
						slog.Debug("main:ui.Button:OnPress")
						toast.Show(ui.ToastContent{
							// ID:          "toast-main-info",
							Type:        ui.ToastType(toasttype.Get()),
							Title:       "Break Info",
							Description: "toast-info",
							Duration:    2 * time.Second,
						})
					},
				},
				ui.Select[uint8]{
					Options: []ui.Option[uint8]{
						{Value: 0, Label: "Plain"},
						{Value: 1, Label: "Info"},
						{Value: 2, Label: "Success"},
						{Value: 3, Label: "Warning"},
						{Value: 4, Label: "Error"},
					},
					Value: toasttype.Get,
					OnChange: func(value uint8) {
						toasttype.Set(value)
					},
					Placeholder: "Select toast type",
					Label:       "main-select-toast-type",
				},
			},
		},
		ui.Toasts{
			Toaster:    toast,
			Placement:  0,
			Reverse:    false,
			Swipe:      0,
			Inset:      0,
			CloseLabel: "main-toast-info-close",
			TestID:     "main-toast-info",
		},
	}
}

func todoList(cx *core.Scope) core.Node {
	type itemRow struct {
		id     int
		action string
		isDone bool
	}
	items := make([]itemRow, 0, 12)
	// for i := range 12 {
	// 	items = append(items, itemRow{i, fmt.Sprintf("task-%02d", i), false})
	// }
	// nextid := core.NewSignal(cx, len(items))
	nextid := core.NewSignal(cx, 1)
	listItem := core.NewSignalFunc(cx, items, nil)
	colors := ui.ColorsOf(cx)
	addTaskText := core.NewSignal(cx, "")
	listAdd := func() {
		task := strings.TrimSpace(addTaskText.Get())
		if task == "" {
			return
		}
		id := nextid.Get()
		nextid.Set(id + 1)
		listItem.Update(func(ir []itemRow) []itemRow {
			return append(ir, itemRow{id, task, false})
		})
		addTaskText.Set("")
	}
	setDone := func(id int, done bool) {
		listItem.Update(func(ir []itemRow) []itemRow {
			next := append([]itemRow(nil), ir...)
			for i := range next {
				if next[i].id == id {
					next[i].isDone = done
				}
			}
			return next
		})
	}
	listRemove := func(id int) {
		listItem.Update(func(ir []itemRow) []itemRow {
			out := ir[:0:0]
			for _, it := range ir {
				if it.id != id {
					out = append(out, it)
				}
			}
			return out
		})
	}
	redc := core.RGB(0xff0000)
	yellowc := core.RGB(0xffff00)
	greenc := core.RGB(0x00ff00)
	return core.List{
		Style: core.Style{
			FlexGrow:    1,
			Borders:     core.All(2),
			CornerRadii: core.AllCorners(10),
			BorderColors: core.EdgeColors{
				Top:    greenc,
				Right:  greenc.Faded(0.5),
				Bottom: greenc.Faded(0.2),
				Left:   greenc.Faded(0.7),
			},
			Gradients: []core.Gradient{
				{
					Kind:     core.GradientLinear,
					Angle:    90,
					ToCorner: core.ToBottomRight,
					Circle:   false,
					Extent:   core.ExtentFarthestSide,
					Center:   core.ObjectPosition{},
					Stops: []core.ColorStop{
						{Color: redc, At: core.Pct(25)},
						{Color: redc.Faded(0.314), At: core.Pct(50)},
						{Color: redc.Faded(0.633), At: core.Pct(75)},
						{Color: yellowc, At: core.Pct(100)},
					},
					Repeating: false,
					Space:     0,
				},
				core.LinearGradient(275, redc, redc.Faded(0.314), redc.Faded(0.633), yellowc),
			},
			Transition: core.Transition{
				Duration: 300 * time.Millisecond,
				Delay:    50 * time.Millisecond,
				Easing:   core.EaseIn,
				// Properties: core.TransitionBorder,
			},
		},
		Count: func() int {
			return len(listItem.Get())
		},
		Key: func(i int) any {
			return listItem.Get()[i].id
		},
		Row: func(cx *core.Scope, row core.ListRow) core.Node {
			index := row.Index()
			item := listItem.Get()[index]
			slog.Info("todoList:row", "row", row, "index", index, "item", item)
			return core.View{
				Style: core.Style{Direction: core.Row, Margin: core.Symmetric(0, 5)},
				Children: []core.Node{
					ui.Checkbox{
						Checked: func() bool {
							for _, x := range listItem.Get() {
								if x.id == index {
									return x.isDone
								}
							}
							return false
						},
						OnChange: func(checked bool) {
							slog.Info("todoList:row:onchange", "items", listItem.Get(), "checked", checked)
							setDone(item.id, checked)
						},
						Text:  item.action,
						Style: core.Style{FlexGrow: 1, FlexShrink: 1},
					},
					ui.IconButton{
						Icon: lucide.Trash,
						// Label: "Button Trash to remove task",
						Variant: ui.IconButtonGhost,
						OnPress: func() {
							listRemove(item.id)
						},
						// TestID: fmt.Sprintf("todo-checkbox-button-x-%d", index),
					},
				},
			}
		},
		EstimatedSize: 44,
		Gap:           0,
		Header: core.View{
			Style: core.Style{Margin: core.All(5)},
			Children: []core.Node{
				ui.Textf(func() string {
					li := listItem.Get()
					done := 0
					for _, it := range li {
						if it.isDone {
							done++
						}
					}
					return fmt.Sprintf("Our current task is %d, with done %d",
						len(li), done)
				}, 12, ui.Regular, colors.Faint),
				core.View{
					Style: core.Style{Direction: core.Row},
					Children: []core.Node{
						ui.Input{
							Style:       core.Style{FlexGrow: 1, Width: core.Pct(70)},
							Value:       addTaskText.Get,
							OnChange:    addTaskText.Set,
							Placeholder: "Add more task!",
							OnSubmit: func(text string) {
								if text == "" {
									return
								}
								listAdd()
							},
						},
						ui.Spacer{},
						ui.Button{Text: "Add Task", Icon: lucide.Plus, OnPress: listAdd},
					},
				},
			},
		},
		Footer: nil,
		Empty: core.View{
			Style: core.Style{
				JustifyContent: core.JustifyCenter,
			},
			Children: []core.Node{
				ui.Text("Empty task, add more task", 24, ui.Bold, colors.Text),
			},
		},
	}
}

func todoList2(cx *core.Scope, nav *base.Nav[string]) base.NavPage {
	type todo struct {
		id   int
		text string
		done bool
	}
	items := core.NewSignalFunc(cx, []todo{}, nil)
	draft := core.NewSignal(cx, "")
	nextID := core.NewSignal(cx, 1)
	colors := ui.ColorsOf(cx)

	add := func() {
		text := strings.TrimSpace(draft.Get())
		if text == "" {
			return
		}
		id := nextID.Get()
		nextID.Set(id + 1)
		items.Update(func(list []todo) []todo {
			return append(list, todo{id: id, text: text})
		})
		draft.Set("")
	}
	setDone := func(id int, done bool) {
		items.Update(func(list []todo) []todo {
			next := append([]todo(nil), list...)
			for i := range next {
				if next[i].id == id {
					next[i].done = done
				}
			}
			return next
		})
	}
	remove := func(id int) {
		items.Update(func(list []todo) []todo {
			out := list[:0:0]
			for _, it := range list {
				if it.id != id {
					out = append(out, it)
				}
			}
			return out
		})
	}
	left := func() int {
		n := 0
		for _, it := range items.Get() {
			if !it.done {
				n++
			}
		}
		return n
	}

	return ui.NavPage(cx, base.NavPage{
		Title: "Todo Example",
		Children: []core.Node{
			core.View{
				Style: core.Style{FlexGrow: 1, FlexShrink: 1, Padding: core.All(24), PaddingEnv: core.EnvAll(core.EnvSafeArea), Gap: 16},
				StyleFunc: func() core.Style {
					return core.Style{Background: colors.Background()}
				},
				Children: []core.Node{
					ui.Text("Todos", 28, ui.Bold, colors.Text),
					core.View{Style: core.Style{Direction: core.Row, Gap: 8}, Children: []core.Node{
						ui.Input{Value: draft.Get, OnChange: draft.Set, Placeholder: "What needs doing?",
							OnSubmit: func(string) { add() }, TestID: "draft", Style: core.Style{FlexGrow: 1}},
						ui.Button{Text: "Add", Icon: lucide.Plus, OnPress: add, TestID: "add"},
					}},
					ui.Textf(func() string { return fmt.Sprintf("%d left", left()) }, 13, ui.Regular, colors.Muted),
					core.List{
						Count:         func() int { return len(items.Get()) },
						Key:           func(i int) any { return items.Get()[i].id },
						EstimatedSize: 44,
						Style:         core.Style{FlexGrow: 1},
						Empty:         ui.Text("Nothing yet. Add one above.", 13, ui.Regular, colors.Muted),
						Row: func(cx *core.Scope, row core.ListRow) core.Node {
							index := row.Index()
							it := items.Get()[index]
							return core.View{Style: core.Style{Direction: core.Row, AlignItems: core.AlignCenter, Gap: 8, Padding: core.Symmetric(8, 0)},
								Children: []core.Node{
									ui.Checkbox{
										Checked: func() bool {
											for _, x := range items.Get() {
												if x.id == it.id {
													return x.done
												}
											}
											return false
										},
										OnChange: func(done bool) { setDone(it.id, done) },
										Text:     it.text,
										TestID:   fmt.Sprintf("todo-%d", it.id),
										Style:    core.Style{FlexGrow: 1, FlexShrink: 1},
									},
									ui.IconButton{Icon: lucide.Trash, Label: "Delete", Variant: ui.IconButtonGhost,
										TestID: fmt.Sprintf("delete-%d", it.id), OnPress: func() { remove(it.id) }},
								}}
						},
					},
				},
			}},
	}, ui.NavHeader{
		OnBack:   func() { nav.Pop(1) },
		Trailing: []core.Node{switchThemeButton(cx)},
	})
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
		Trailing: []core.Node{switchThemeButton(cx)},
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
					ui.Menu{
						Trigger: ui.Button{Icon: lucide.EllipsisVertical, Variant: ui.ButtonGhost},
						Children: []core.Node{
							ui.MenuItem{
								// switchTheme(cx),
								Text:     "Switch theme",
								Icon:     lucide.SunMoon,
								OnSelect: switchTheme(cx),
							},
							ui.MenuItem{Text: "Front", Icon: lucide.House, OnSelect: func() { nav.Push("Front") }},
						},
					},
				},
			})
		case "Setting":
			return settingInfo(cx, nav, urlsetting)
		case "TodoList2":
			return todoList2(cx, nav)
		}
		colors := ui.ColorsOf(cx)
		infos := []core.Node{}
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
				switchThemeButton(cx),
			},
		})
	})
}

func switchTheme(cx *core.Scope) func() {
	return func() {
		appearance := core.AppearanceDark
		if cx.Dark() {
			appearance = core.AppearanceLight
		}
		cx.SetAppearance(appearance)
	}

}

func switchThemeButton(cx *core.Scope) core.Node {
	return ui.IconButton{Icon: lucide.SunMoon, Label: "Switch-Theme",
		Variant: ui.IconButtonRaised, OnPress: switchTheme(cx),
	}
}

func main() { nsgo.Run(Root) }
