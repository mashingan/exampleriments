package main

import (
	"fmt"
	"time"
)

func heh1s(i int) int {
	time.Sleep(1 * time.Second)
	return i
}

func main() {
	counter := make(chan int)
	start := time.Now()
	tocount := 0
	for i := 0; i < 10; i++ {
		go func(num int, ch chan int) {
			ch <- heh1s(num)
		}(i, counter)
		tocount++
	}
	length := 0
	for tocount > 0 {
		length += <-counter
		tocount--
	}
	fmt.Printf("total count %d with elapsed total time: %v", length, time.Now().Sub(start))
}
