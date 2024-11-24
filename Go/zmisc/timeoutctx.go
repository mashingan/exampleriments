package main

import (
	. "context"
	"log"
	"sync"
	"time"
)

func successAfter(iter int, x time.Duration, ctx Context, w *sync.WaitGroup) {
	defer w.Done()
	log.Printf("goroutine %d to wait for %v\n", iter, x)
	start := time.Now()
	select {
	case <-time.After(x):
		log.Println("goroutine success:", iter)
	case <-ctx.Done():
		log.Printf("goroutine %d context done after: %v\n", iter, time.Since(start))
	}
}

func main() {
	log.SetFlags(log.Lshortfile | log.LstdFlags)
	ctx, cancel := WithTimeout(Background(), 1*time.Second)
	defer cancel()
	var wg sync.WaitGroup
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go successAfter(i, time.Duration(i)*200*time.Millisecond, ctx, &wg)
	}

	start := time.Now()
	select {
	case <-time.After(2 * time.Second):
		log.Println("main success waiting for 2 seconds")
	case <-ctx.Done():
		log.Println("main abruptly done after:", time.Since(start))
	}
	wg.Wait()
}
