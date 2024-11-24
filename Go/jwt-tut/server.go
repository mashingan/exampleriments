package main

import (
	"log"
	"net/http"

	"github.com/gorilla/mux"
)

func main() {
	log.Println("Server starting up at port 3000")

	connectDatabase()
	route := mux.NewRouter()
	addAppRoutes(route)
	log.Fatal(http.ListenAndServe(":3000", route))
}
