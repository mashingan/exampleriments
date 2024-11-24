package main

import (
	"fmt"
	"log"

	"github.com/mashingan/smapping"
)

type UserEntity struct {
	ID       string
	Email    string
	Name     string
	Password string
	Token    string
}

type UserLoginDTO struct {
	Email string
	Token string
}

type UserGeneralDTO struct {
	ID    string
	Email string
	Name  string
	Token string
}

func main() {
	fmt.Println("Hello, playground")
	ent := UserEntity{
		ID:       "entityID",
		Email:    "user@entity",
		Name:     "entoma",
		Password: "cz",
		Token:    "entoken",
	}
	usLogin := UserLoginDTO{}
	err := smapping.FillStruct(&usLogin, smapping.MapFields(&ent))
	if err != nil {
		log.Fatalf("failed map: %v", err)
	}
	log.Println("usLogin:", usLogin)

	usgen := UserGeneralDTO{}
	err = smapping.FillStruct(&usgen, smapping.MapFields(&ent))
	if err != nil {
		log.Fatalf("failed map: %v", err)
	}
	log.Println("usgen:", usgen)
}
