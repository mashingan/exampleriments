package main

import (
	"github.com/labstack/echo/v4"

	"main/api"
)

func main() {
	e := echo.New()
	e.GET("/", hello)
	e.GET("/api", api.Api)

	e.Start(":3000")
}
