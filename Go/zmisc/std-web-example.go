package main

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"
	"net/http/httptest"
	"sync/atomic"
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
	w.Write(jsret)
}

type Count struct {
	CurrentCount uint64 `json:"currentCount"`
}

func countUp(count *uint64) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		atomic.AddUint64(count, 1)
		jsret, err := json.Marshal(Count{CurrentCount: *count})
		if err != nil {
			errWriter(w, err)
			return
		}
		w.Header()["Content-Type"] = []string{"application/json"}
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
		atomic.AddUint64(count, ^uint64(0))
		jsret, err := json.Marshal(Count{CurrentCount: *count})
		if err != nil {
			errWriter(w, err)
			return
		}
		w.Header()["Content-Type"] = []string{"application/json"}
		wrt, err := w.Write(jsret)
		if err != nil {
			errWriter(w, err)
			return
		}
		log.Printf("Countdown success writing: %d bytes", wrt)
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

func test(handle http.HandlerFunc, req *http.Request, count *uint64) {
	w := httptest.NewRecorder()
	handle(w, req)
	resp := w.Result()
	checkresponse(resp, *count)
}

func main() {
	var count uint64

	requp := httptest.NewRequest("GET", "/countup", nil)
	reqdown := httptest.NewRequest("GET", "/countdown", nil)
	test(countUp(&count), requp, &count)
	test(countDown(&count), reqdown, &count)

}
