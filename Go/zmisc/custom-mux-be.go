package main

import (
	"log"

	"net/http"
)

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		log.Println("Method:", req.Method)
		w.WriteHeader(http.StatusOK)
		w.Header().Set("Content-Type", "text/html")
		w.Write([]byte("ok"))
	})
	log.Fatal(http.ListenAndServe(":3007", nil))
}
