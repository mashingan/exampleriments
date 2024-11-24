package main

import (
	"context"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"sync"
)

func runActualService(srvc chan<- *http.Server) {
	srv := &http.Server{
		Addr: ":3001",
	}
	mux := http.NewServeMux()
	mux.HandleFunc("/the-path", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", r.Header.Get("Content-Type"))
		w.WriteHeader(http.StatusOK)
		io.Copy(w, r.Body)
	})
	srv.Handler = mux
	srvc <- srv
	srv.ListenAndServe()
}

func mainHandler(w http.ResponseWriter, r *http.Request) {
	rr, _ := http.NewRequest("GET", "http://localhost:3001/the-path", r.Body)
	rr.Header.Set("Content-Type", r.Header.Get("Content-Type"))
	resp, err := http.DefaultClient.Do(rr)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte(err.Error()))
		return
	}
	w.WriteHeader(resp.StatusCode)
	io.Copy(w, resp.Body)
}

func runClient(w *sync.WaitGroup) {
	defer w.Done()
	vals := url.Values{
		"key-1": {"val-1"},
	}
	resp, err := http.PostForm("http://localhost:3000/", vals)
	if err != nil {
		log.Println(err)
		return
	}
	if resp.StatusCode != http.StatusOK {
		log.Printf("unexpected status, got %d\n", resp.StatusCode)
	}
	fmt.Print("the response body is: ")
	io.Copy(os.Stdout, resp.Body)
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	ctx := context.Background()
	actualSrv := make(chan *http.Server, 1)
	go runActualService(actualSrv)

	mux := http.NewServeMux()
	mux.HandleFunc("/", mainHandler)
	thisSrv := http.Server{
		Addr:    ":3000",
		Handler: mux,
	}
	go thisSrv.ListenAndServe()
	var w sync.WaitGroup
	w.Add(1)
	go runClient(&w)

	w.Wait()
	asrv := <-actualSrv
	thisSrv.Shutdown(ctx)
	asrv.Shutdown(ctx)
}
