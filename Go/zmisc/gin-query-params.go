package main

import (
	"log"
	"net/http"
	"net/http/httptest"
	"strconv"
	"testing"

	"github.com/gin-gonic/gin"
	//"gotest.tools/v3/assert"
	"github.com/stretchr/testify/assert"
)

func main() {
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	r := gin.Default()
	r.GET("/map-param", func(c *gin.Context) {
		page, err := strconv.Atoi(c.DefaultQuery("page", "1"))
		if err != nil {
			log.Println("page err:", err)
		}
		log.Printf("page: %d type %T\n", page, page)
		view, err := strconv.Atoi(c.DefaultQuery("view", "20"))
		if err != nil {
			log.Println("view err:", err)
		}
		log.Printf("view: %d type %T\n", view, view)
		filters := c.QueryMap("filters")
		log.Printf("filters: %v type %T\n", filters, filters)

		c.String(http.StatusOK, "OK")
	})
	r.GET("/list-param", func(c *gin.Context) {
		status := c.QueryArray("status")
		log.Println("Status:", status)
		c.String(http.StatusOK, "OK")
	})

	t := &testing.T{}
	w := httptest.NewRecorder()
	req, _ := http.NewRequest("GET",
		"/map-param?page=3&view=10&filters[nama_lengkap]=rozi&filters[alamat]=kayu",
		nil)
	r.ServeHTTP(w, req)
	assert.Equal(t, 200, w.Code)

	w = httptest.NewRecorder()
	req, _ = http.NewRequest("GET",
		"/list-param?status=ok&status=status-2&status=confirm",
		nil)
	r.ServeHTTP(w, req)
	assert.Equal(t, 200, w.Code)
}
