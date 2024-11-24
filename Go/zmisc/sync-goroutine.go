package main

import (
	"fmt"
	"time"
)

func main() {
	event1Ready := make(chan struct{}, 1)
	event2Ready := make(chan struct{}, 1)
	event3Ready := make(chan struct{}, 1)
	fmt.Println("Hello, playground")
	go func(ch chan<- struct{}) {
		// do op 1
		fmt.Println("operation 1: ongoing")
		time.Sleep(100 * time.Millisecond)
		ch <- struct{}{}
	}(event1Ready)
	go func(ch1 <-chan struct{}, ch2 chan<- struct{}) {
		// do some preparation
		fmt.Println("prepare operation 2")
		time.Sleep(50 * time.Millisecond)
		<-ch1
		// do some
		fmt.Println("operation 2: ongoing")
		time.Sleep(50 * time.Millisecond)
		ch2 <- struct{}{}
	}(event1Ready, event2Ready)
	go func(ch2 <-chan struct{}, ch3 chan<- struct{}) {
		fmt.Println("prepare for operation 3")
		time.Sleep(100 * time.Millisecond)
		<-ch2
		fmt.Println("operation 3: finishing")
		time.Sleep(50 * time.Millisecond)
		ch3 <- struct{}{}
	}(event2Ready, event3Ready)
	fmt.Println("===run async waiting====")
	<-event3Ready
}
