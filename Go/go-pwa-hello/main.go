package main

// ref: https://go-app.dev/getting-started

import (
	"log"
	"net/http"

	"github.com/maxence-charriere/go-app/v10/pkg/app"
)

type hello struct {
	app.Compo
}

// func (h *hello) Render() app.UI {
// 	return app.H1().Text("Hello world")
// }

func (h *hello) Render() app.UI {
	return app.Div().Body(
		app.H1().Class("title").Text("Build UI with Go"),
		app.P().Class("text").Text("With Go because it's simply simple awesome"),
	)
}

func main() {
	app.Route("/", func() app.Composer { return &hello{} })
	app.RunWhenOnBrowser()
	http.Handle("/", &app.Handler{
		Name:        "Hello",
		Description: "A Hello World! Example",
	})
	if err := http.ListenAndServe(":3000", nil); err != nil {
		log.Fatal(err)
	}
}
