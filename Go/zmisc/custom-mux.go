package main

import (
	"fmt"
	"log"
	"net/http"
	"net/url"
	"os"
)

func isPreFlight(w http.ResponseWriter, req *http.Request) bool {
	if req.Method == http.MethodOptions {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "*")
		w.Header().Set("Access-Control-Allow-Headers", "*")
		w.Write(nil)
		return true
	}
	return false
}

func writeErr(w http.ResponseWriter, label string, err error) {
	log.Println(label, ":", err)
	w.WriteHeader(http.StatusInternalServerError)
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Write([]byte("Error: " + err.Error()))
}

func proxy(label, behost string) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		if isPreFlight(w, req) {
			return
		}
		if req.Method != http.MethodPost {
			w.WriteHeader(http.StatusMethodNotAllowed)
			w.Write([]byte(req.Method + " not allowed!"))
			return
		}
		client := &http.Client{}
		url, err := url.Parse(behost)
		if err != nil {
			msg := fmt.Sprintf("url parse %s err: %s", behost, err.Error())
			log.Println(msg)
			w.WriteHeader(http.StatusInternalServerError)
			w.Header().Set("Content-Type", "text/html")
			w.Write([]byte(msg))
			return
		}
		url.Path = req.URL.Path
		req.RequestURI = ""
		req.URL = url
		resp, err := client.Do(req)
		if err != nil {
			writeErr(w, label, err)
			return
		}
		w.Header().Set("Access-Control-Allow-Origin", "*")
		resp.Write(w)
	}
}

func main() {
	mux := http.NewServeMux()
	/*
		mux.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
			if isPreFlight(w, req) {
				return
			}
			w.WriteHeader(http.StatusOK)
			w.Header().Set("Access-Control-Allow-Origin", "*")
			w.Write([]byte("Welcome"))
		})
	*/
	behost := os.Getenv("BEHOST")
	mux.HandleFunc("/paggr-be/advise", proxy("advise", behost))
	mux.HandleFunc("/paggr-be/fulfillment", proxy("fulfillment", behost))
	mux.HandleFunc("/paggr-be/void", proxy("void", behost))
	log.Fatal(http.ListenAndServe(":3006", mux))
}
