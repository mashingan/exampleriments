package main

import (
	"fmt"
	"time"
)

func main() {
	go func() {
		fmt.Println("hello world")
	}()
	time.Sleep(time.Second * 5)
}
