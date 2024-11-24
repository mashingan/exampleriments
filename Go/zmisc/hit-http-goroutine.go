package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"runtime"
	"sync"
)

type Task struct {
	ID        int    `json:"id"`
	UserID    int    `json:"user_id"`
	Title     string `json:"title"`
	Completed bool   `json:"completed"`
}

func main() {
	var t Task

	var wg sync.WaitGroup
	wg.Add(100)

	title := make([]string, 100)
	ch := make(chan struct{}, 10)
	for i := 0; i < 100; i++ {
		ch <- struct{}{}
		fmt.Println(runtime.NumGoroutine())
		go func(i int, c chan struct{}) {
			defer wg.Done()
			res, err := http.Get(fmt.Sprintf("https://jsonplaceholder.typicode.com/todos/%d", i))
			if err != nil {
				log.Fatal(err)
			}
			defer res.Body.Close()
			if err := json.NewDecoder(res.Body).Decode(&t); err != nil {
				log.Fatal(err)
			}
			title[i] = t.Title
			fmt.Println(i, ": title:", t.Title)
			<-ch
		}(i, ch)
	}
	wg.Wait()
	for i, t := range title {
		fmt.Println(i, ":", t)
	}
}
