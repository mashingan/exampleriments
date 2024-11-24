package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
)

func echo(w http.ResponseWriter, r *http.Request) {
	io.WriteString(w, fmt.Sprintf("%v", r.URL))
}

func handler(w http.ResponseWriter, r *http.Request) {
	if r.URL.RawQuery != "42" {
		io.WriteString(w, "invalid truth of life")
		return
	}
	echo(w, r)
}

func main() {
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe(":6543", nil))
}
