# Drawer Navigation

## Table of content

1. [Introduction](#introduction)
2. [Empty window](#empty-window)
3. [AppBar navigation](#appbar-navigation)
4. [Draw modal navigation](#draw-modal-navigation)
5. [Navigation item and content paging](#navigation-item-and-content-paging)
6. [Pattern of router](#pattern-of-router)
7. [Afterword](#afterword)

## Introduction

In this example, we'll explore the `component` package which implement
Material design, specifically the `Nav` and `ModalNav`.

The `Nav` acts as navigation itself while the `ModalNav` is the drawer that'll
be rendered on top of current page.

We will go through several iteration of adding functionalities from the simplest
to complete usable navigation and its drawer. Navigation relies on the `AppBar`
so we will also add it though it's only the simplest form of the `AppBar`
for the sake adding capability of `AppBarNavigationClicked`. In case users
want to learn/see the example of complete `AppBar` working, they can check
the [`AppBar` tutorial](../appbar/tutorial-appbar-modallayer.md).

This example will use snippet style explanations so anyone who read this
example need to follow the steps to reproduce the same result.
At the end of example, we'll give a complete ready runnable code
for impatient readers.

[^top](#drawer-navigation)

## Empty window

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

[^top](#drawer-navigation)

## AppBar navigation

We got our bare window doing nothing, now we're going to add the `AppBar` on it.

```patch
++4: 	"image/color"
++10: 	"gioui.org/op/clip"
++11: 	"gioui.org/op/paint"
++12: 	"gioui.org/widget"
++13: 	"gioui.org/widget/material"
++14: 	"gioui.org/x/component"
++15: 	"golang.org/x/exp/shiny/materialdesign/icons"



--23: 	var ops op.Ops
++30: 	var (
++31: 		ops   op.Ops
++32: 		modal = component.NewModal()
++33: 		th    = material.NewTheme()
++34: 		ab    = component.NewAppBar(modal)
++35: 	)
++36: 	navIcon, _ := widget.NewIcon(icons.NavigationApps)
++37: 	ab.NavigationIcon = navIcon
++38: 	ab.Title = "Nav"


--30: 			thelayout(gtx)
++45: 			thelayout(gtx, th, ab)


--36: func thelayout(gtx layout.Context) layout.Dimensions {
++51: func thelayout(gtx layout.Context, th *material.Theme, ab *component.AppBar) layout.Dimensions {
++52: 	abar := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
++53: 		return ab.Layout(gtx, th, "Menu", "Actions")
++54: 	})
++55: 	layout.Flex{Axis: layout.Vertical}.Layout(gtx,
++56: 		abar,
++57: 		layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++58: 			defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
++59: 			paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
++60: 			paint.PaintOp{}.Add(gtx.Ops)
++61: 			return layout.Dimensions{Size: gtx.Constraints.Max}
++62: 		}))

```

[main.go](./03.appbar-navigation/main.go)

Above we showed the patch of changes from previous subchapter example to
current example.
In case we never see the patch format file. The first `--` or `++` means the
change whether we delete (`--`) or we add (`++`) the line. For modifying
the same line it will delete the old line and add the new change hence we see above

```patch
--30: thelayout(gtx)
++45: thelayout(gtx, th, ab)
```

Which literally means, delete the line 30 in previous example and add to line
45. The numbering between two files quite different due to above insertions
hence the numbering doesn't exactly same between two files but usually we only
need to follow the format `--N` with `N` as the line in previous file to change.

At very first we only add the dependencies to be used in below patch later. With
`gofmt` it's usually added automatically but if it's not, we can add it manually.

The next changes are adding the necessary component to be rendered.
We add `Modal`, `Theme`, and `AppBar` together with navigation icon.
In order to show those component in `thelayout` function, we modified to
accept the `Theme` and `AppBar` in the argument.

The last changes are the layouting itself. We needed that to ensure our
component, in this case the `AppBar` would be rendered as we wanted.
We rendered the layout with vertical flex with the above is the `AppBar`
itself and the rest is simple rectangle white transparent.

[^top](#drawer-navigation)


## Draw modal navigation

Our next iteration is to have a modal draw when we click the `AppBar` navigation
continuing our last example after added the no-op `AppBar`.

But before starting out, let's refactor our current code

```patch
++6: 	"time"


++30: type appcomp struct {
++31: 	*component.ModalLayer
++32: 	*component.AppBar
++33: 	*component.ModalNavDrawer
++34: 	*component.NavDrawer
++35: 	navAnim component.VisibilityAnimation
++36: }
++37: 

--31: 		ops   op.Ops
--32: 		modal = component.NewModal()
--33: 		th    = material.NewTheme()
--34: 		ab    = component.NewAppBar(modal)
++40: 		ops      op.Ops
++41: 		modal    = component.NewModal()
++42: 		nav      = component.NewNav("Navigation Drawer", "This is navigation")
++43: 		modalNav = component.ModalNavFrom(&nav, modal)
++44: 		th       = material.NewTheme()
++45: 		ab       = component.NewAppBar(modal)
++46: 		anim     = component.VisibilityAnimation{
++47: 			State:    component.Invisible,
++48: 			Duration: time.Millisecond * 250,
++49: 		}
++54: 	ac := appcomp{
++55: 		AppBar:         ab,
++56: 		ModalLayer:     modal,
++57: 		ModalNavDrawer: modalNav,
++58: 		NavDrawer:      &nav,
++59: 		navAnim:        anim,
++60: 	}
```

Above, we created an `appcomp` struct to have an object that's convenient
to bring our components to many scope of functions and blocks.
The additional differences of our current refactor are adding the `Nav`,
`ModalNavDrawer`, `NavDrawer`, `VisibilityAnimation`.
With these changes, it'd working as previous, except for unused `ac` variable
here. It's the same as previous because even though we defined the additional
component, our layout renderer is still using previous one.

So to complete it, we need to change `thelayout` function too.

```patch
--45: 			thelayout(gtx, th, ab)
++67: 			thelayout(gtx, th, ac)


--51: func thelayout(gtx layout.Context, th *material.Theme, ab *component.AppBar) layout.Dimensions {
++73: func thelayout(gtx layout.Context, th *material.Theme, ac appcomp) layout.Dimensions {
++74: 	for _, event := range ac.AppBar.Events(gtx) {
++75: 		switch event := event.(type) {
++76: 		case component.AppBarNavigationClicked:
++77: 			ac.ModalNavDrawer.Appear(gtx.Now)
++78: 			ac.navAnim.Disappear(gtx.Now)
++79: 		case component.AppBarContextMenuDismissed:
++80: 			_ = event
++81: 		case component.AppBarOverflowActionClicked:
++82: 			//
++83: 		}
++84: 	}
++85: 	paint.Fill(gtx.Ops, th.Palette.Bg)
--53: 		return ab.Layout(gtx, th, "Menu", "Actions")
++87: 		return ac.AppBar.Layout(gtx, th, "Menu", "Actions")
++88: 	})
++89: 	content := layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++90: 		return layout.Flex{}.Layout(gtx,
++91: 			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
++92: 				gtx.Constraints.Max.X /= 3
++93: 				return ac.NavDrawer.Layout(gtx, th, &ac.navAnim)
++94: 			}),
++95: 			layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++96: 				defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
++97: 				paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
++98: 				paint.PaintOp{}.Add(gtx.Ops)
++99: 				return layout.Dimensions{Size: gtx.Constraints.Max}
++100: 
++101: 			}))
--55: 	layout.Flex{Axis: layout.Vertical}.Layout(gtx,
--56: 		abar,
--57: 		layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
--58: 			defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
--59: 			paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
--60: 			paint.PaintOp{}.Add(gtx.Ops)
--61: 			return layout.Dimensions{Size: gtx.Constraints.Max}
--62: 		}))
++103: 	layout.Flex{Axis: layout.Vertical}.Layout(gtx, abar, content)
++104: 	ac.ModalLayer.Layout(gtx, th)
```

The above changes ensured we supplied the `ac` variable to `thelayout`
and changed our `thelayout` signature to accept `appcomp` instead of
`*component.AppBar`.

The next we polled the events in `AppBar` which have kinds of

1. `AppBarNavigationClicked`: When the icon nav is clicked
2. `AppBarContextMenuDismissed`: When exiting the contextual menu (check tutorial [AppBar contextual](../appbar/tutorial-appbar-modallayer.md#contextual-appbar)).
3. `AppBarOverflowActionClicked`: When any list in overflow clicked. In this example we have no overflows.

Our concern of rendering the modal is when `AppBarNavigationClicked` so
we explicitly called `ac.ModalNavDrawer.Appear` immediately (`gtx.Now`) with
setting the `ac.navAnim.Disappear` immediately after modal appear.
Since we don't do anything with contextual and overflows, we ignore it but
we discard `_ = event` to ensure it can be compiled.

Another notable changes are changings the rectangle of white transparent
combined with rendering `ac.NavDrawer.Layout` which we put in variable as
`content`.

Lastly we put the layout of `abar` and `content` vertically and after that
we rendered the `ac.ModalLayer.Layout` to ensure our modal will be came out
when we clicked the `AppBar` navigation icon.

Here's the complete [main.go](./04.draw-modal-navigation/main.go) for this
sub-chapter.

[^top](#drawer-navigation)

## Navigation item and content paging

Now we have the modal navigation drawer, it's time to add some items in it
and having it switch to appropriate content displayer when we navigate to
the selected item.

Since it's navigation, it's only natural to have content different depend on
what menu/route it's navigated so we'll also give its interaction based on
selected menu.

First of all, let's add some item to the `Nav`

```patch
++13: 	"gioui.org/unit"

++37: 	page    int

++63: 	nav.AddNavItem(component.NavItem{Tag: 0, Name: "Nav"})
++64: 	nav.AddNavItem(component.NavItem{Tag: 1, Name: "Page 1"})
++65: 	ac.page = 0

--67: 			thelayout(gtx, th, ac)
++72: 			thelayout(gtx, th, &ac)
```

Above we put a new field `page` in `appcomp` struct. This `page` will hold
our current chosen route to render the content. The second part is our adding
the `NavItem` to the navigation list to be chosen. The tag here is used
for our page navigation assigned to `appcomp.page`. Lastly we modified
`thelayout` to accept pointer of `appcomp` instance instead of the object
instance itself.

Now for the meat of route rendering.

```patch
--73: func thelayout(gtx layout.Context, th *material.Theme, ac appcomp) layout.Dimensions {
++78: func thelayout(gtx layout.Context, th *material.Theme, ac *appcomp) layout.Dimensions {

++90: 	if ac.ModalNavDrawer.NavDestinationChanged() {
++91: 		ac.page = ac.ModalNavDrawer.CurrentNavDestination().(int)
++92: 		if ac.page == 0 {
++93: 			ac.AppBar.Title = "Nav"
++94: 		} else if ac.page == 1 {
++95: 			ac.AppBar.Title = "Page 1"
++96: 		}
++97: 	}
--89: 	content := layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
--90: 		return layout.Flex{}.Layout(gtx,
--91: 			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
--92: 				gtx.Constraints.Max.X /= 3
--93: 				return ac.NavDrawer.Layout(gtx, th, &ac.navAnim)
--94: 			}),
--95: 			layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
--96: 				defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
--97: 				paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
--98: 				paint.PaintOp{}.Add(gtx.Ops)
--99: 				return layout.Dimensions{Size: gtx.Constraints.Max}
--100: 
--101: 			}))
++102: 	drawer := layout.Rigid(func(gtx layout.Context) layout.Dimensions {
++103: 		gtx.Constraints.Max.X /= 3
++104: 		return ac.NavDrawer.Layout(gtx, th, &ac.navAnim)
++106: 	var content layout.FlexChild
++107: 	if ac.page == 0 {
++108: 		content = layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++109: 			return layout.Flex{}.Layout(gtx,
++110: 				drawer,
++111: 				layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++112: 					defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
++113: 					paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
++114: 					paint.PaintOp{}.Add(gtx.Ops)
++115: 					return layout.Dimensions{Size: gtx.Constraints.Max}
++116: 
++117: 				}))
++118: 		})
++119: 
++120: 	} else if ac.page == 1 {
++121: 		content = layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++122: 			return layout.Flex{}.Layout(gtx, drawer,
++123: 				layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++124: 					return layout.UniformInset(unit.Dp(8)).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
++125: 						return material.H2(th, "Content page 1").Layout(gtx)
++126: 					})
++127: 				}))
++128: 		})
++129: 
++130: 	}
```

In these changes above, while it seemed big changes, but it's actually simple.
We can summary the change as

1. Check if `ac.ModalDrawer.NavDestinationChanged()`
2. If yes, change current page and also `AppBar.title`
3. We move the previous `content` to first part of `if-else` clause when page is 0.
4. We also refactored out the `NavDrawer.layout` to its dedicated `drawer` variable.
5. Chose which one to put in the `content` whether it's page 0 or 1.
6. The `content` of page 1 simply `H3` of `"Content page 1"`.
7. Render the `content` like previous.

A notable change apart from adding the navigation route is changing `thelayout`
signature from accepting object to pointer. It's because our `ac.page` is
changed inside `thelayout` but with object it will be always 0 instead of
navigated page. That's because the `ac.page` is not shared as it's only a value
not reference. So in order persist our change across `thelayout` invocation
we need to make it as reference by changing it to pointer.

Here is the complete [main.go](./05.navigation-item-content/main.go).

[^top](#drawer-navigation)

## Pattern of router

If we scrutinized our previous changes, we see several parts of guarding
which page it's in then change the specific part accordingly. While the
previous only has two `NavItem`s, it begins to hard to maintain when
there are so many pages and/or content.

In order to make it easier and scalable, several pattern we can see are

1. Adding `NavItem` to `Nav`.
2. Checking `NavDestinationChanged` and update the `AppBar.title` and `ac.page`.
3. Checking which page we're in and choose which `content` to rendered in layout.

With these pattern we recognized, we have an object that should be able to

1. Register the `NavItem` to `Nav`
2. Switching to `CurrentNavDestination` which in it update `AppBar.title` and current `page`.
3. Render the layout `content`.

Using above observations, we apply the changes below

```patch
++31: type contenter interface {
++32: 	pageName() string
++33: 	render(*material.Theme) layout.FlexChild
++34: }
++35: 
```

In this part, we defined an `contenter` which will tell the `pageName` for
updating our `AppBar.title` and `render` for displaying our `content`.

```patch
++43: 	pages   []contenter
++44: }
++45: 
++46: func (ac *appcomp) register(ct contenter) {
++47: 	nvi := component.NavItem{
++48: 		Tag:  len(ac.pages),
++49: 		Name: ct.pageName(),
++50: 	}
++51: 	ac.NavDrawer.AddNavItem(nvi)
++52: 	ac.pages = append(ac.pages, ct)
++53: }
++54: 
++55: func (ac *appcomp) toPage(n int) {
++56: 	ac.page = n
++57: 	ac.AppBar.Title = ac.pages[n].pageName()
++58: }
```

This part of changes simplify to have the `appcomp.register` the `contenter` page
to be added to selection item menu as `NavItem`.

```patch
++59: 
++60: type emptyContent struct{}
++61: 
++62: var _ contenter = emptyContent{}
++63: 
++64: func (emptyContent) pageName() string { return "Nav" }
++65: func (emptyContent) render(_ *material.Theme) layout.FlexChild {
++66: 	return layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++67: 		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
++68: 		paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
++69: 		paint.PaintOp{}.Add(gtx.Ops)
++70: 		return layout.Dimensions{Size: gtx.Constraints.Max}
++71: 
++72: 	})
++73: }
++74: 
++75: type content1 struct{}
++76: 
++77: var _ contenter = content1{}
++78: 
++79: func (content1) pageName() string { return "Page 1" }
++80: func (content1) render(th *material.Theme) layout.FlexChild {
++81: 	return layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++82: 		return layout.UniformInset(unit.Dp(8)).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
++83: 			return material.H2(th, "Content page 1").Layout(gtx)
++84: 		})
++85: 	})
```

This part using each type of object for navigation content which implements the
`contenter` interface. The rendering parts are moved from `thelayout` to each
of its page.

```patch
--63: 	nav.AddNavItem(component.NavItem{Tag: 0, Name: "Nav"})
--64: 	nav.AddNavItem(component.NavItem{Tag: 1, Name: "Page 1"})
++111: 	(&ac).register(emptyContent{})
++112: 	(&ac).register(content1{})
```

Refactored to use the `register` method instead of manually adding `NavItem`
with `AddNavItem`.

```patch
--91: 		ac.page = ac.ModalNavDrawer.CurrentNavDestination().(int)
--92: 		if ac.page == 0 {
--93: 			ac.AppBar.Title = "Nav"
--94: 		} else if ac.page == 1 {
--95: 			ac.AppBar.Title = "Page 1"
--96: 		}
++139: 		ac.toPage(ac.ModalNavDrawer.CurrentNavDestination().(int))
```

Also refactored to switch `toPage` instead of guarding which page and
manually set the `AppBar.title`.

```patch
--106: 	var content layout.FlexChild
--107: 	if ac.page == 0 {
--108: 		content = layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
--109: 			return layout.Flex{}.Layout(gtx,
--110: 				drawer,
--111: 				layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
--112: 					defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
--113: 					paint.ColorOp{Color: color.NRGBA{}}.Add(gtx.Ops)
--114: 					paint.PaintOp{}.Add(gtx.Ops)
--115: 					return layout.Dimensions{Size: gtx.Constraints.Max}
--116: 
--117: 				}))
--118: 		})
--119: 
--120: 	} else if ac.page == 1 {
--121: 		content = layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
--122: 			return layout.Flex{}.Layout(gtx, drawer,
--123: 				layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
--124: 					return layout.UniformInset(unit.Dp(8)).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
--125: 						return material.H2(th, "Content page 1").Layout(gtx)
--126: 					})
--127: 				}))
--128: 		})
--129: 
--130: 	}
++149: 	content := layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
++150: 		return layout.Flex{}.Layout(gtx,
++151: 			drawer,
++152: 			ac.pages[ac.page].render(th),
++153: 		)
++154: 	})
```

Finally, our `content` simply gotten from `render(th)` for each `contenter`.
Without the need to guard our `ac.page`, we simply chose our current `page`
and having it to `render` itself.

The complete changes can be found in [main.go](06.pattern-of-router/main.go).

[^top](#drawer-navigation)

## Afterword

In this example, we went through the journey of adding functionality to our
navigation bar starting from empty window to navigating to content from our
selected menu in our modal drawer.
In actuality, we can also add display our menu without the modal, ie. having it
rendered in all pages (tentative).
Also having icon in our navigation item selection list
(this can be exercise for readers) which we omitted in this example.

[^top](#drawer-navigation)
