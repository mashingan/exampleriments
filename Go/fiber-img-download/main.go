package main

import (
	"fmt"
	"net/http"

	"github.com/gofiber/fiber/v2"
	qr "github.com/skip2/go-qrcode"
)

func setupApp() *fiber.App {
	app := fiber.New()
	app.Get("/test-qr-me", func(c *fiber.Ctx) error {
		qrcode, err := qr.Encode("hello world", qr.Medium, 4096)
		if err != nil {
			return err
		}
		c.Set("Content-Disposition", `attachment;filename="qrtest.png"`)
		c.Set("Content-Type", "image/png")
		c.Set("Content-Length", fmt.Sprintf("%d", len(qrcode)))
		return c.Status(http.StatusOK).Send(qrcode)
	})
	return app
}

func main() {
	fmt.Println("vim-go")
	setupApp().Listen(":3000")
}
