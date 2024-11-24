package main

import (
	"fmt"
	"sync"
)

func main() {
	ar := []int{1, 2, 3, 4, 5, 6, 7, 8}
	ch := make(chan int)
	var wg sync.WaitGroup
	sum := 0
	go func() {
		for v := range ch {
			sum += v
		}
	}()
	for _, v := range ar {
		wg.Add(1)
		go func(n int, w *sync.WaitGroup, c chan int) {
			defer w.Done()
			if n%2 == 0 {
				fmt.Println("Event value", n)
				c <- n
			}
		}(v, &wg, ch)
	}
	wg.Wait()
	fmt.Println(sum)

}
