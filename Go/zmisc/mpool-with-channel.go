package main

import (
	"log"
	"sync"
)

type bigObject struct {
	id        int
	something string
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	pool := make(chan *bigObject, 10)
	for i := 0; i < cap(pool); i++ {
		bo := &bigObject{id: i}
		pool <- bo
	}
	var wg sync.WaitGroup
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func() {
			log.Println("loop:", i)
			defer wg.Done()
			bo := <-pool
			defer func() {
				pool <- bo
			}()
			log.Println("Using:", bo.id)
			log.Println("Releasing:", bo.id)
		}()
	}
	wg.Wait()
}
