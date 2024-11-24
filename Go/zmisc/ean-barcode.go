package main

import (
	"fmt"
	"github.com/boombuler/barcode"
	"github.com/boombuler/barcode/ean"
	"image/png"
	"io"
	"log"
	"net/http"
)

func handleErr(w http.ResponseWriter, status int, errmsg string) {
	w.WriteHeader(status)
	io.WriteString(w, errmsg)
}

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		q := req.URL.Query()
		if len(q["code"]) == 0 {
			handleErr(w, http.StatusNotFound,
				fmt.Sprintf("code not supplied in"))
			return
		}
		bar, err := ean.Encode(q["code"][0])
		if err != nil {
			handleErr(w, http.StatusInternalServerError,
				fmt.Sprintf("%s", err.Error()))
			return
		}
		bar2, err := barcode.Scale(bar, 200, 200)
		if err != nil {
			handleErr(w, http.StatusInternalServerError,
				fmt.Sprintf("%s", err.Error()))
			return
		}
		w.Header().Set("Content-Type", "image/png")
		png.Encode(w, bar2)
	})
	log.Fatal(http.ListenAndServe(":3000", nil))
}
