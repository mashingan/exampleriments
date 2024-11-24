package main

import (
	"context"
	"io"
	"log"
	"net/http"
	"os"
	"os/signal"
)

func main() {
	//var srv http.Server
	srv := http.Server{
		Addr: ":3000",
	}
	idleConnsClosed := make(chan struct{})
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		io.WriteString(w, "hello 異世界!")
	})
	go func() {
		sigint := make(chan os.Signal, 1)
		signal.Notify(sigint, os.Interrupt)
		<-sigint

		if err := srv.Shutdown(context.Background()); err != nil {
			log.Printf("Http server shutdown: %v", err)
		}
		close(idleConnsClosed)
	}()
	if err := srv.ListenAndServe(); err != http.ErrServerClosed {
		log.Fatalf("Http server ListenAndServe: %v", err)
	}
	<-idleConnsClosed
}
