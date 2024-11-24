package main

import (
	"context"
	"log"
	"sync"

	"github.com/go-redis/redis/v8"
	"github.com/mashingan/localredis"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	var w sync.WaitGroup
	w.Add(1)
	go func() {
		defer w.Done()
		localredis.ListenAndServe(":8099")
	}()

	//defer localredis.Close()
	client := redis.NewClient(&redis.Options{
		//Addr:     "localhost:6379",
		// Addr: ":3000",
		Addr: ":8099",
	})
	pong, err := client.Ping(context.Background()).Result()
	if err != nil {
		log.Fatal("Error:", err)
	}
	defer client.Close()
	log.Println("pong:", pong)

	if err = client.Set(context.Background(), "name", "Elliot", 0).Err(); err != nil {
		log.Println("err set:", err)
	}

	val, err := client.Get(context.Background(), "name").Result()
	if err != nil {
		log.Println("err get:", err)
	}
	log.Println(val)
	localredis.Close()
	w.Wait()
}
