package main

import (
	"io"
	"log"
	"net/http"
	"os"
	"os/signal"
)

func signalCatcher(waiter chan bool) {
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt)
	s := <-c
	log.Println("Got signal:", s)
	waiter <- true
}

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		io.WriteString(w, "hello world!")
	})
	waiter := make(chan bool, 1)
	go signalCatcher(waiter)
	go func() {
		log.Fatal(http.ListenAndServe(":3000", nil))
	}()
	<-waiter
}
