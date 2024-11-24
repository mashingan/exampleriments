package main

import (
	"bytes"
	"fmt"
	"log"
	"net/http"
	"net/http/httptest"

	"github.com/gin-gonic/gin"
)

type outer1 struct {
	//Reqbody reqbody `json:"reqbody" binding:"structonly"`
	Reqbody reqbody `json:"reqbody"`
}

type reqbody struct {
	SomeField string `json:"field" binding:"required,len=16,numeric"`
}

func main() {
	fmt.Println("Hello, playground")
	r := gin.New()
	r.POST("/single-struct", func(c *gin.Context) {
		var body reqbody
		if err := c.ShouldBindJSON(&body); err != nil {
			log.Println(err)
			c.JSON(http.StatusBadRequest, gin.H{"message": err.Error()})
			return
		}
		c.String(http.StatusOK, "ok")
	})
	r.POST("/level-struct", func(c *gin.Context) {
		var body outer1
		if err := c.ShouldBindJSON(&body); err != nil {
			log.Println(err)
			c.JSON(http.StatusBadRequest, gin.H{"message": err.Error()})
			return
		}
		c.String(http.StatusOK, "ok")
	})

	// failed request
	w := httptest.NewRecorder()
	rawlevels := []byte(`
{ "reqbody": {
    "field": "1234567890123456"
    }
}`)
	req, _ := http.NewRequest("POST", "/level-struct",
		bytes.NewBuffer(rawlevels))
	r.ServeHTTP(w, req)
	fmt.Println("code:", w.Code)
	fmt.Println("status:", w.Result().Status)

	// success request
	w = httptest.NewRecorder()
	rawsingle := []byte(`{"field": "1234567890123456"}`)
	req, _ = http.NewRequest("POST", "/single-struct", bytes.NewBuffer(rawsingle))
	r.ServeHTTP(w, req)
	fmt.Println("code success:", w.Code)
	fmt.Println("status success:", w.Result().Status)
}
