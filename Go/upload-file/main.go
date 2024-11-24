package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"mime/multipart"
	"net/http"
	"strings"

	"github.com/gin-gonic/gin"
)

const (
	uploadpage = `./index.html`
	upfail     = `
	<head>
	<meta http-equiv="Refresh" content="1; URL="http://locahost:9000/">
	</head>
	<body><p>Upload failed</p></body>`
	maxmem int64 = 500 * 1024 * 1024
)

var otherService *gin.Engine

func startOtherService(ready chan struct{}) {
	// r := gin.New()
	r := gin.Default()
	r.POST("/upload", func(c *gin.Context) {
		form, err := c.MultipartForm()
		if err != nil {
			log.Println(err)
			c.JSON(http.StatusBadRequest, gin.H{"message": err.Error()})
			return
		}
		errmsgs := []string{}
		for _, file := range form.File {
			for _, f := range file {
				err := c.SaveUploadedFile(f, fmt.Sprintf("./upload/%s", f.Filename))
				if err != nil {
					errmsgs = append(errmsgs, err.Error())
				}
			}
		}
		if len(errmsgs) > 0 {
			msgs := strings.Join(errmsgs, "\n")
			log.Println(msgs)
			c.JSON(http.StatusInternalServerError, gin.H{"message": msgs})
			return
		}
		// c.String(http.StatusOK, "upload file success")

		// _, mhead, err := c.Request.FormFile("file")
		// if err != nil {
		// 	log.Println(err)
		// 	c.JSON(http.StatusInternalServerError, gin.H{"message": err.Error()})
		// 	return
		// }
		// if err := c.SaveUploadedFile(mhead, fmt.Sprintf("./upload/%s", mhead.Filename)); err != nil {
		// 	log.Println(err)
		// 	c.JSON(http.StatusInternalServerError, gin.H{"message": err.Error()})
		// 	return
		// }

		c.JSON(http.StatusOK, nil)
	})
	ready <- struct{}{}
	r.Run(":9001")
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	ready := make(chan struct{}, 1)
	go startOtherService(ready)
	r := gin.Default()
	// r.Static("/", ".")
	r.GET("/", func(c *gin.Context) {
		c.File(uploadpage)
	})
	r.POST("/upload", func(c *gin.Context) {
		err := c.Request.ParseMultipartForm(maxmem)
		if err != nil {
			log.Println(err)
			c.Header("Content-Type", "text/html")
			c.String(http.StatusRequestEntityTooLarge, upfail)
			return
		}
		log.Println(c.Request.Header)
		// contentType := c.Request.Header.Get("Content-Type")
		// mfile, mhead, err := c.Request.FormFile("file")
		buff, contentType, err := createRequestForm(c.Request)
		if err != nil {
			log.Println(err)
			c.Header("Content-Type", "text/html")
			c.String(http.StatusInternalServerError, upfail)
			return
		}
		// log.Println("file name:", mhead.Filename)
		// log.Println("size:", mhead.Size)
		// defer mfile.Close()

		resp, err := http.Post("http://localhost:9001/upload",
			contentType,
			buff)
		// mfile)
		if err != nil {
			log.Println(err)
			c.Header("Content-Type", "text/html")
			c.String(http.StatusInternalServerError, upfail)
			return
		}
		defer resp.Body.Close()
		if resp.StatusCode >= 400 {
			err := fmt.Errorf("Error dependency status: %s", resp.Status)
			log.Println(err)
			c.Header("Content-Type", "text/html")
			c.String(http.StatusInternalServerError, upfail)
			return
		}
		c.Header("Location", "/")
		c.String(http.StatusSeeOther, "")
	})
	r.GET("/upload", func(c *gin.Context) {
	})

	<-ready
	r.Run(":9000")
	// r.GET
}

func createRequestForm(r *http.Request) (*bytes.Buffer, string, error) {
	mfile, mhead, err := r.FormFile("file")
	if err != nil {
		log.Println(err)
		return nil, "", err
	}
	defer mfile.Close()
	log.Println("file name:", mhead.Filename)
	log.Println("size:", mhead.Size)
	buff := new(bytes.Buffer)
	pw := multipart.NewWriter(buff)
	w, err := pw.CreateFormFile("file", mhead.Filename)
	if err != nil {
		log.Println(err)
		return buff, "", err
	}
	io.Copy(w, mfile)
	pw.Close()
	return buff, pw.FormDataContentType(), err
}
