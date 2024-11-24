package main

import (
	"log"
	"sync"
	"time"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile | log.Lmicroseconds)

	consumer := make(chan int, 1)
	var wconsum sync.WaitGroup
	wconsum.Add(1)

	go func(w *sync.WaitGroup) {
		defer w.Done()
		for val := range consumer {
			log.Println("consume:", val)
			time.Sleep(500 * time.Millisecond)
		}
	}(&wconsum)

	var wprod sync.WaitGroup
	for i := 0; i < 10; i++ {
		wprod.Add(1)
		go func(num int, w *sync.WaitGroup) {
			defer w.Done()
			log.Println("request to send:", num)
			consumer <- num
			log.Println("success sending num:", num)
		}(i, &wprod)
	}

	wprod.Wait()
	close(consumer)
	wconsum.Wait()
}
