package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"sync"
	"sync/atomic"
	"time"
)

func errWriter(w http.ResponseWriter, err error) {
	type errmsg struct {
		Message string `json:"errmsg"`
	}
	jsret, err := json.Marshal(errmsg{Message: err.Error()})
	if err != nil {
		log.Printf("Error sending error: %s", err.Error())
		return
	}
	w.WriteHeader(http.StatusMethodNotAllowed)
	w.Header().Set("Content-Type", "application/json")
	w.Write(jsret)
}

type Count struct {
	CurrentCount uint64 `json:"currentCount"`
}

func countUp(count *uint64) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			errWriter(w, fmt.Errorf("Not supported method"))
			return
		}
		atomic.AddUint64(count, 1)
		jsret, err := json.Marshal(Count{CurrentCount: *count})
		if err != nil {
			errWriter(w, err)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		wrt, err := w.Write(jsret)
		if err != nil {
			errWriter(w, err)
			return
		}
		log.Printf("Countup success writing: %d bytes", wrt)
	}
}

func countDown(count *uint64) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			errWriter(w, fmt.Errorf("Not supported method"))
			return
		}
		atomic.AddUint64(count, ^uint64(0))
		jsret, err := json.Marshal(Count{CurrentCount: *count})
		if err != nil {
			errWriter(w, err)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		wrt, err := w.Write(jsret)
		if err != nil {
			errWriter(w, err)
			return
		}
		log.Printf("Countdown success writing: %d bytes", wrt)
	}
}

func closeServer(srv *http.Server) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			errWriter(w, fmt.Errorf("Not supported method"))
			return
		}
		log.Println("Server closing")
		w.Header().Set("Content-Type", "application/json")
		w.Write([]byte(`{"msg": "server is closing"}`))
		w.(http.Flusher).Flush()
		srv.Shutdown(context.Background())
	}
}

func checkresponse(resp *http.Response, expectedCount uint64) {
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	if resp.StatusCode != http.StatusOK {
		log.Fatalf("Invalid status return expected 200: got %d", resp.StatusCode)
	}
	contentType := resp.Header.Get("Content-Type")
	if contentType != "application/json" {
		log.Fatalf("Invalid content type expected 'application/json'"+
			" got %s", contentType)
	}
	countres := Count{}
	log.Println(string(body))
	err = json.Unmarshal(body, &countres)
	if err != nil {
		log.Fatal(err)
	}
	if countres.CurrentCount != expectedCount {
		log.Fatalf("Expected %d but got response count %d",
			expectedCount, countres.CurrentCount)
	}
}

func main() {
	var count uint64

	// setup the server
	server := http.Server{
		Addr:              ":3000",
		ReadHeaderTimeout: time.Second * 5,
		WriteTimeout:      time.Second * 2,
	}
	mux := http.NewServeMux()
	mux.HandleFunc("/countup", countUp(&count))
	mux.HandleFunc("/countdown", countDown(&count))
	mux.HandleFunc("/close", closeServer(&server))
	server.Handler = mux

	// run the server
	var wg sync.WaitGroup
	wg.Add(2)
	ready := make(chan bool, 1)
	go func(w *sync.WaitGroup) {
		defer w.Done()
		ready <- true
		if err := server.ListenAndServe(); err != http.ErrServerClosed {
			log.Fatal("Http server ListenAndServe:", err)
		}
	}(&wg)

	// test and close
	go func(w *sync.WaitGroup) {
		defer w.Done()
		<-ready
		resp, err := http.Post("http://localhost:3000/countup",
			"application/json", nil)
		if err != nil {
			log.Println("POST error:", err)
		}
		checkresponse(resp, 1)
		resp, err = http.Post("http://localhost:3000/countdown",
			"application/json", nil)
		if err != nil {
			log.Println("POST error:", err)
		}
		checkresponse(resp, 0)
		customClient := &http.Client{
			Timeout: time.Millisecond * 500,
		}
		resp, err = customClient.Post("http://localhost:3000/close",
			"application/json", nil)
		if err != nil {
			log.Printf("Closing error: %s", err.Error())
		}
		log.Println("OK")
	}(&wg)

	wg.Wait()
}
