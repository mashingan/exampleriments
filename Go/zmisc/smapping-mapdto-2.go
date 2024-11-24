package main

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/mashingan/smapping"
)

type UserEntity struct {
	ID       string `json:"id"`
	Email    string `json:"email" login:"email"`
	Name     string `json:"name"`
	Password string `json:"password"`
	Token    string `json:"token" login:"token"`
}

func main() {
	fmt.Println("Hello, playground")
	ent := UserEntity{
		ID:       "entityID",
		Email:    "user@entity",
		Name:     "entoma",
		Password: "passcz",
		Token:    "entoken",
	}

	jslogin, err := json.Marshal(smapping.MapTags(&ent, "login"))
	if err != nil {
		log.Fatalf("failed map: %v", err)
	}
	log.Println("jslogin:", string(jslogin))

	entmap := smapping.MapTags(&ent, "json")
	delete(entmap, "password")
	jsusgen, err := json.Marshal(entmap)
	if err != nil {
		log.Fatalf("failed map: %v", err)
	}
	log.Println("jsusgen:", string(jsusgen))
}
