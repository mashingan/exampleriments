package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/gorilla/websocket"
)

var upgrader = &websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func homepage(w http.ResponseWriter, r *http.Request) {
	// fmt.Fprintf(w, "Home page")
	http.ServeFile(w, r, "index.html")
}

func wsEndpoint(w http.ResponseWriter, r *http.Request) {
	upgrader.CheckOrigin = func(r *http.Request) bool { return true }
	// fmt.Fprintf(w, "Hello world")

	ws, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
	}
	err = ws.WriteMessage(1, []byte("Hi client!"))
	if err != nil {
		log.Println(err)
	}
	wsRead(ws)
}

func wsRead(conn *websocket.Conn) {
	for {
		messageType, p, err := conn.ReadMessage()
		if err != nil {
			log.Println(err)
			return
		}
		fmt.Println(string(p))

		if err := conn.WriteMessage(messageType, p); err != nil {
			log.Println(err)
			return
		}
	}
}

func setupRoutes() {
	http.HandleFunc("/", homepage)
	http.HandleFunc("/ws", wsEndpoint)
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	fmt.Println("etst ws")
	setupRoutes()
	log.Println(http.ListenAndServe(":8080", nil))
}
