package main

import (
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
	"sync"

	"github.com/gin-gonic/gin"
)

var (
	tgtsvc, proxysvc *gin.Engine
)

func runProxy(w *sync.WaitGroup) {
	// proxysvc = gin.Default()
	proxysvc = gin.Default()
	// proxysvc.Any("/", func(c *gin.Context) {
	proxysvc.POST("/uproxy", func(c *gin.Context) {
		log.Println("run in proxy")
		log.Println("reqpath:", c.Request.URL)
		log.Println("reqmeth:", c.Request.Method)
		log.Println("headers:", c.Request.Header)
		director := func(req *http.Request) {
			r := c.Request
			req = r
			req.URL.Scheme = "http"
			req.URL.Path = "/uproxy"
			req.URL.Host = "localhost:9002"
			req.Header["my-header"] = []string{r.Header.Get("my-header")}
		}
		proxy := &httputil.ReverseProxy{Director: director}
		proxy.ServeHTTP(c.Writer, c.Request)
	})
	go func() {
		proxysvc.Run(":9001")

	}()
	w.Done()
}

func runTarget(w *sync.WaitGroup) {
	// tgtsvc = gin.Default()
	tgtsvc = gin.Default()
	tgtsvc.POST("/uproxy", func(c *gin.Context) {
		log.Println("run in target")
		log.Println("reqpath:", c.Request.URL)
		log.Println("headers:", c.Request.Header)
		c.JSON(http.StatusOK, nil)
	})
	go func() {
		tgtsvc.Run(":9002")
	}()
	w.Done()
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	var ready sync.WaitGroup
	ready.Add(2)
	go runProxy(&ready)
	go runTarget(&ready)
	// ready.Done()
	r := gin.Default()
	// r := gin.Default()
	r.POST("/uproxy", func(c *gin.Context) {
		purl, _ := url.Parse("http://localhost:9002/uproxy")
		proxyurl, _ := url.Parse("http://localhost:9001/uproxy")
		client := &http.Client{
			Transport: &http.Transport{
				Proxy: http.ProxyURL(proxyurl),
			},
		}
		// client := &http.Client{}
		purlstr := purl.String()
		log.Println("purlstr:", purlstr)
		log.Println("proxystr:", proxyurl.String())
		req, _ := http.NewRequest("POST", purlstr, nil)
		resp, err := client.Do(req)
		if err != nil {
			log.Println(err)
			c.String(http.StatusFailedDependency, err.Error())
			return
		}
		if resp.StatusCode >= http.StatusBadRequest {
			log.Println(resp.Status)
			c.String(resp.StatusCode, "nothing")
			return
		}
		c.String(http.StatusOK, "ok")
	})
	// w := httptest.NewRecorder()
	// req, _ := http.NewRequest("POST", "/uproxy", nil)
	ready.Wait()
	// r.ServeHTTP(w, req)
	// t := &testing.T{}
	// assert.Equal(t, w.Code, http.StatusOK)
	// time.Sleep(2 * time.Second)
	r.Run(":9000")
}
