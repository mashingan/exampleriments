package main

import (
	"log"
	"net/http"
)

func main() {
	log.Fatal(http.ListenAndServe(":8080", http.FileServer(http.Dir("/home/rahmat/.choosenim/toolchains/nim-2.2.0/doc/html"))))
}
