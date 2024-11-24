package main

import (
	"io"
	"log"
	"net/http"
	"os"
)

func fetchMp3() (*os.File, error) {
	mp3, err := os.Open("01.FLARE (Another Intro Version).mp3")
	if err != nil {
		return nil, err
	}
	return mp3, nil
}

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, _ *http.Request) {
		mp3, err := fetchMp3()
		if err != nil {
			w.WriteHeader(http.StatusNotFound)
			io.WriteString(w, "No resource")
			return
		}
		defer mp3.Close()
		w.Header().Set("Content-Type", "audio/mpeg")
		/* to encourage browser to download
		w.Header().Set("Content-Disposition",
			"attachment;filename=flare.mp3")
		*/
		io.Copy(w, mp3)

	})

	log.Fatal(http.ListenAndServe(":3000", nil))
}
