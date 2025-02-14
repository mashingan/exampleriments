package main

import (
	"flag"
	"log"
	"os"

	"gio.test/experiments/components/pages"
	"gio.test/experiments/components/pages/about"
	"gio.test/experiments/components/pages/appbar"
	"gio.test/experiments/components/pages/discloser"
	"gio.test/experiments/components/pages/menu"
	"gio.test/experiments/components/pages/navdrawer"
	"gio.test/experiments/components/pages/textfield"
	"gioui.org/app"
	"gioui.org/font/gofont"
	"gioui.org/op"
	"gioui.org/text"
	"gioui.org/widget/material"
)

func main() {
	flag.Parse()
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	go func() {
		window := new(app.Window)
		window.Option(app.Title("Component"))
		if err := loop(window); err != nil {
			log.Fatal(err)
		}
		os.Exit(0)
	}()
	app.Main()
}

func loop(w *app.Window) error {
	var ops op.Ops
	th := material.NewTheme()
	th.Shaper = text.NewShaper(text.WithCollection(gofont.Collection()))

	router := pages.NewRouter()
	router.Register(0, appbar.New(&router))
	router.Register(1, navdrawer.New(&router))
	router.Register(2, textfield.New(&router))
	router.Register(3, menu.New(&router))
	router.Register(4, discloser.New(&router))
	router.Register(5, about.New(&router))
	for {
		switch e := w.Event().(type) {
		case app.DestroyEvent:
			return e.Err
		case app.FrameEvent:
			ops.Reset()
			gtx := app.NewContext(&ops, e)
			router.Layout(gtx, th)
			e.Frame(gtx.Ops)
		}
	}
}
