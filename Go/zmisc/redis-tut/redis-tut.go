package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"

	"github.com/go-redis/redis/v8"
	"github.com/mashingan/smapping"
)

type author struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

type redisAuth struct {
	Username string `json:"username"`
	Token    string `json:"token"`
}

type signInResponse struct {
	AccessToken      string `json:"access_token" out:"accessToken"`
	ExpiresIn        int64  `json:"expires_in" out:"expiresIn"`
	RefreshExpiresIn int64  `json:"refresh_expires_in" out:"refreshExpiresIn"`
	RefreshToken     string `json:"refresh_token" out:"refreshToken"`
	TokenType        string `json:"token_type" out:"tokenType"`
	IdToken          string `json:"id_token" out:"idToken"`
	NotBeforePolicy  int64  `json:"not-before-policy" out:"notBeforePolicy"`
	SessionState     string `json:"session_state" out:"sessionState"`
	Scope            string `json:"scope" out:"scope"`
}

func main() {
	fmt.Println("Golang redis tutorial")

	client := redis.NewClient(&redis.Options{
		//Addr:     "localhost:6379",
		Addr:     "10.172.24.38:80",
		Password: "",
		DB:       0,
	})

	pong, err := client.Ping(context.Background()).Result()
	log.Println(pong, err)
	res, err := client.Get(context.Background(), "LKP-AUTH:rahmat").
		Result()
	if err != nil {
		log.Println(err)
	}
	log.Println("res:", res)
	if res == "" {
		return
	}

	auth := redisAuth{}
	if err := json.Unmarshal([]byte(res), &auth); err != nil {
		log.Fatal(err)
	}
	res, err = client.Get(context.Background(), auth.Token).Result()
	if err != nil {
		log.Println(err)
	}
	log.Println("redis res:", res)
	if res == "" {
		return
	}
	sign := signInResponse{}
	if err := json.Unmarshal([]byte(res), &sign); err != nil {
		log.Fatal(err)
	}
	mapout := smapping.MapTags(&sign, "out")
	log.Println("mapout:", mapout)
	//log.Println("sign:", sign)
	/*

		if err = client.Set("name", "Elliot", 0).Err(); err != nil {
			fmt.Println("err set:", err)
		}

		val, err := client.Get("name").Result()
		if err != nil {
			fmt.Println("err get:", err)
		}
		fmt.Println(val)

		js, err := json.Marshal(author{
			Name: "Elliot",
			Age:  25,
		})
		if err != nil {
			fmt.Println(err)
		}

		if err = client.Set("id1234", js, 0).Err(); err != nil {
			fmt.Println("err set id:", err)
		}
		val, err = client.Get("id1234").Result()
		if err != nil {
			fmt.Println("err set id:", err)
		}
		fmt.Println(val)
	*/
}
