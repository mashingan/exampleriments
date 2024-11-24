package api

import (
	"net/http"

	"github.com/labstack/echo/v4"
)

func Api(c echo.Context) error {
	return c.String(http.StatusOK, "from api")
}
