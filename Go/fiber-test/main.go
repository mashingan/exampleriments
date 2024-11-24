package main

import (
	"fmt"
	"net/http"

	"github.com/gofiber/fiber/v2"
)

type respmsg struct {
	Code int
	Msg  string
}

func setupApp() *fiber.App {
	app := fiber.New(fiber.Config{
		ErrorHandler: func(ctx *fiber.Ctx, err error) error {
			code := http.StatusInternalServerError
			msg := "Internal server error"

			if e, ok := err.(*fiber.Error); ok {
				code = e.Code
				msg = e.Message
			}
			return ctx.Status(code).JSON(respmsg{
				Code: code,
				Msg:  msg,
			})
		},
	})
	app.Get("/", func(c *fiber.Ctx) error {
		return fiber.NewError(403, "Forbidden")
	})
	return app
}

func main() {
	fmt.Println("vim-go")
	setupApp().Listen(":3000")
}
