# Appbar and ModalLayer

## Table of content

1. [Introduction](#introduction)
2. [Empty window](#empty-window)
3. [Adding No-op Appbar](#adding-no-op-appbar)
4. [AppBar icon](#appbar-icon)
5. [AppBar Actions](#appbar-actions)
    1. [AppBar interactivity](#appbar-interactivity)
    2. [AppBar Overflow](#appbar-overflow)
    3. [AppBar event handling](#appbar-event-handling)
6. [Contextual AppBar](#contextual-appbar)
7. [Afterword](#afterword)

## Introduction

In this example, we'll explore the `component` package which implement
Material design, specifically the `Appbar` and `ModalLayer`.

Why the `Appbar` and `ModalLayer` combined? Because the `Appbar` itself only
show the specific bar with context maximum size width and fixed high.

`Appbar` by itself doesn't much anything but combined with `ModalLayer` 
(and other components) we'll see interesting result. The `ModalLayer`
works as modal when we click overflow menu.

This example will use snippet style explanations so anyone who read this
example need to follow the steps to reproduce the same result.
At the end of example, we'll give a complete ready runnable code
for impatient readers.

[^top](#appbar-and-modallayer)

## Empty Window

Let's dive immediately to empty gui windows

```go
package main

import (
	"os"

	"gioui.org/app"
	"gioui.org/layout"
	"gioui.org/op"
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

func loop(w *app.Window) error {
	var ops op.Ops
	for {
		switch e := w.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			gtx := app.NewContext(&ops, e)
			thelayout(gtx)
			e.Frame(gtx.Ops)
		}
	}
}

func thelayout(gtx layout.Context) layout.Dimensions {
	return layout.Dimensions{Size: gtx.Constraints.Max}
}
```

[main.go](./02.empty-loop/main.go)

This is the empty gui window and doing nothing. This the skeleton block.
We specifically will draw our ui `thelayout` function
but that's just entry point of drawing.

[^top](#appbar-and-modallayer)

## Adding No-op Appbar

Since we have our empty gui. Let's add the Appbar, with doing nothing in it.

```go
// import (
// ...
// ...

	"gioui.org/widget/material"
	"gioui.org/x/component"
//)
func loop(w *app.Window) error {
    var op op.Ops
    modal := component.NewModal()
    appbar := component.NewAppBar(modal)
    th := material.NewTheme()
// ...
// ...
// ...
		case app.FrameEvent:
			gtx := app.NewContext(&ops, e)
			thelayout(gtx, th, appbar)
			e.Frame(gtx.Ops)


// -- func thelayout(gtx layout.Context) layout.Dimension {
// ++
func thelayout(gtx layout.Context, th *material.Theme, appbar *component.AppBar) layout.Dimension {
    appbar.Layout(gtx, th, "Menu", "Actions")
    return layout.Dimension{Size: gtx.Constraint.Max}
}

```

[main.go](./03-adding-noop-appbar/main.go)

Above is the change we need to add doing nothing app bar. The part of displaying
the appbar is in function call of `appbar.Layout(gtx, th, "Menu", "Actions")`.
If we remove that line, we will see nothing because it's never rendered to our
layout.

`AppBar` need `Modal` when defined because it will show the overflow menu. We'll
get to it later.

[^top](#appbar-and-modallayer)

## AppBar Icon

This part, we will add icon and some informational title for our
AppBar


```go

package main

// import (
// ++
	"gioui.org/widget"
	"golang.org/x/exp/shiny/materialdesign/icons"
// )
// ...
// ...
// ...
	var ops op.Ops
	modal := component.NewModal()
	appbar := component.NewAppBar(modal)
	settingIcon, _ := widget.NewIcon(icons.ActionSettingsApplications)
	appbar.NavigationIcon = settingIcon

// ...
// ...
// ...

func thelayout(gtx layout.Context, th *material.Theme, appbar *component.AppBar) layout.Dimensions {
	appbarpos := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		return appbar.Layout(gtx, th, "Menu", "Actions")
	})
	layout.Flex{Axis: layout.Vertical}.Layout(gtx, appbarpos)
	return layout.Dimensions{Size: gtx.Constraints.Max}
}
```

[main.go](./04-draw-with-content/main.go)

The most contrast of above changes is the layout of drawing context. We
show the `layout.Flex` for `appbarpos`. Our layout only consist
of one child (`layout.FlexChild`) that's the `appbarpos` which is rigid
and specifically take position with width and height.

Without this layouting, if we revert `thelayout` function to previous version,
we'll see the icon position be placed in middle of our gui window, which that's
not what we want to see.

For the icon and title, we simply add the attribute to our AppBar which the
icon for material is supplied in package widget and golang.x icons

[^top](#appbar-and-modallayer)

## AppBar Actions

Now we see our `AppBar` nicely hanging on top of page layout. Let's interact
with it with `AppBarAction`. But before that, let's refactor our current code
a bit.

```go

//++ we put the components and states vars in our singleton
type appcomponent struct {
	*component.ModalLayer
	*component.AppBar
	*material.Theme
	plusBtn widget.Clickable
}

// ...
// ...
// ...
// ...

	th := material.NewTheme()

	appstate := appcomponent{
		ModalLayer: modal,
		AppBar:     appbar,
		Theme:      th,
	}

// ...
// ...
// ...
			// -- thelayout(gtx, th, appbar)
			thelayout(gtx, appstate)


// -- func thelayout(gtx layout.Context, appbar *component.AppBar) layout.Dimensions {
func thelayout(gtx layout.Context, appstate appcomponent) layout.Dimensions {
	appbarpos := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
		// -- return appBar.Layout(gtx, th, "Menu", "Actions")
		return appstate.AppBar.Layout(gtx, appstate.Theme, "Menu", "Actions")
	})
	layout.Flex{Axis: layout.Vertical}.Layout(gtx, appbarpos)
	return layout.Dimensions{Size: gtx.Constraints.Max}
```

We're changing from scattered variables in `thelayout` function to be
a struct `appcomponent`. This is still the same as before and if we
run our current refactored code, it will give same result as
chapter [4](#appbar-icon).

### AppBar interactivity

Now, the keys of interactivity for `AppBar` are in `AppBarAction`
,`OverflowAction` and method `AppBar.SetActions`.
`AppBarAction` is the element of available in `AppBar` header rendered when
area is still available. When it's not available, it'll become `OverflowAction`
that's consist of the label and tag (for interactivity).
Lastly after we defined our actions, we need call the `SetActions` to
register it to our `AppBar`.
And to receipt the interaction, we need to follow for its available events
in `AppBar`.

Let's add it to our code

```go

	appstate := appcomponent{
		ModalLayer: modal,
		AppBar:     appbar,
		Theme:      th,
	}
	appbarActions := []component.AppBarAction{
		{
			OverflowAction: component.OverflowAction{
				Name: "Create",
				Tag:  &appstate.plusBtn,
			},
			Layout: func(gtx layout.Context, bg, fg color.NRGBA) layout.Dimensions {
				ic, _ := widget.NewIcon(icons.ContentCreate)
				btn := component.SimpleIconButton(bg, fg, &appstate.plusBtn, ic)
				btn.Background = bg
				btn.Color = fg
				if appstate.plusBtnCreating {
					btn.Color = color.NRGBA{R: 0xff, A: 0xff}
				}
				return btn.Layout(gtx)
			},
		},
	}
	appbarOverflows := []component.OverflowAction{} // we'll use later
	appstate.AppBar.SetActions(appbarActions, appbarOverflows)
// ...
// ...
// ...
func thelayout(gtx layout.Context, appstate appcomponent) layout.Dimensions {
	for _, event := range appstate.AppBar.Events(gtx) {
		switch event := event.(type) {
		case component.AppBarNavigationClicked:
			_ = event
		case component.AppBarContextMenuDismissed:
		case component.AppBarOverflowActionClicked:
		}
	}
// ...
// ...
// ...
```

Above we see the loop inquiry for events in `AppBar` and the possible events
in there.
`AppBarNavigationClicked` is the icon on the left of title while the
`AppBarOverflowActionClicked` is when we display the overflow list menu.

### AppBar Overflow

If we apply our changes above, try to change the width of window so it becomes
narrow enough to hide the icon pencil (`icons.ContentCreate`), we'll see
a three vertical dot (⋮) on the most right but it doesn't do anything.
It's because we still haven't add its layout rendering so it's still no-op.

Now, let's add some list to the menu and render it.

```go
// ...
// ...
// ...
	// -- plusBtn widget.Clickable
	plusBtn, ov1btn widget.Clickable

// ...
// ...
// ...
	appbarOverflows := []component.OverflowAction{
		{
			Name: "Overflow 1",
			Tag:  &appstate.ov1btn,
		},
	}
	appstate.AppBar.SetActions(appbarActions, appbarOverflows)
// ...
// ...
// ...
	layout.Flex{Axis: layout.Vertical}.Layout(gtx, appbarpos)
	appstate.ModalLayer.Layout(gtx, appstate.Theme)
	return layout.Dimensions{Size: gtx.Constraints.Max}
```

Above, as we stated, we added a menu named as `"Overflow 1"` and called
the method `ModalLayer.Layout` to render the overflow menu list.

### AppBar event handling

Now we've done with layouting and its various components, let's continue
on handling of each button behaviour.
We want to make each time the pencil icon pressed, it will toggle between
color green or default.

```go
// ...
// ...
// ...
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
// ...
// ...
// ...
```

We replaced the original `Layout` field for `AppBarAction` labeled "Create"
by checking the `plusBtn.Clicked`.

Now try to narrow the window width again so the pencil icon offloaded to 
overflow menu, try clicking it. What will happen?

<details>
<summary>Spoiler</summary>
It will do nothing. No color toggling for pencil.
</details>

If you tried above (or peek the answer in spoiler), you'll know whether
anything happened if we clicked a label in overflow menu. In order to
process the event, we need to handle it accordingly.

```go
// ...
// ...
// ...
		case component.AppBarOverflowActionClicked:
			wc, ok := event.Tag.(*widget.Clickable)
			if !ok {
				continue
			}
			wc.Click()
// ...
// ...
// ...
```

Now it's behaving the same whether it's as `AppBarAction` or `OverflowAction`.

The final code for this chapter can be found [here](./05-appbar-actions/main.go).

[^top](#appbar-and-modallayer)

## Contextual AppBar

For contextual app bar header, it's almost the same as `AppBarAction` with 
difference in `SetContextualActions` instead of `SetActions` in
normal `AppBarAction`. To enable the contextual mode, we either need to
`ToggleContextual` or `StartContextual` which both won't start again if it's
already in contextual mode.
Here's the catch, we can define different contextual title depend on what's
the title it's supplied when calling `ToggleContextual` or `StartContextual`.

To add the contextual, here's what we change

```go
// ...
// ...
// ...

type appcomponent struct {
// ...
// ...
// ...
	plusBtn, ov1btn, ctxbtn widget.Clickable
	plusBtnCreating         bool
	whichbtn                map[any]string
}
// ...
// ...
// ...
func loop(w *app.Window) error {
// ...
// ...
// ...
	appstate.whichbtn = make(map[any]string)
	appstate.whichbtn[&appstate.plusBtn] = "plusbtn"
	appstate.whichbtn[&appstate.ov1btn] = "ov1btn"
	appstate.whichbtn[&appstate.ctxbtn] = "ctxbtn"
// ...
// ...
// ...
	appbarActions := []component.AppBarAction{
// ...
// ...
// ...
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
// ...
// ...
// ...
}

func thelayout(gtx layout.Context, appstate appcomponent) layout.Dimensions {
// ...
// ...
// ...
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
// ...
// ...
// ...
}
```

[main.go]()

Above changes, quite big so let's tread each carefully.

First of all, we add the `ctxbtn` for `widget.Clickable` in `appcomponent`
as button toggle of entering/exiting contextual mode.
Also we add `whichbtn` to track which `*widget.Clickable` in overflow menu.

Before the loop inside `loop` function, we map the `*widget.Clickable`
to `whichbtn` with each identifiable string. Note that we only use strings
here and also it's hardcoded but in actuality we can use enum or const'ed
strings since it's read only.
We also add a second `AppBarAction` with icon play which will be our trigger
to `ToggleContextual`. This also adding `OverflowAction` with label "Contextual"
and will act the same as its `AppBarAction`.
Lastly we `SetContextualActions` to be displayed in gallery of contextual app bar.

In `thelayout` function, we adjust handling for `AppBarOverflowActionClicked`.
Previously we regenerate `Click` for each `*widget.Clickable` but in
specific case (`ctxbtn`), we just immediately `ToggleContextual` to make it work.
We do that by matching the button according `whichbtn` map.

In contextual mode/demo, we have the icon heart (❤️) as `AppBarAction` but when
we click it won't work, now try to narrow the width window until the ⋮ appears
and click the "Fav" label. We will find it will toggle out the contextual mode.
While we know how to fix that, this will be left as exercise to readers.

[^top](#appbar-and-modallayer)

## Afterword

We reach our last journey of exploring `AppBar`. While this is last journey in
this occasion, it doesn't mean this is the end.
There's so many things we can explore more, like for example
toggling navigation icon or even choosing which icon to show,
many different contextual modes based on which component we interact.

Many thanks for readers who reading this tutorial as exploration journal.

[^top](#appbar-and-modallayer)
