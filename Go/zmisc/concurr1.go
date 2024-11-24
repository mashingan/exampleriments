package main

import (
	"fmt"
	"time"
)

func main() {
	ochan := make(chan string)
	go func(oc chan string) {
		for {
			fmt.Println(<-oc)
		}
	}(ochan)
	for i := 0; i < 5; i++ {
		ochan <- fmt.Sprintf("msg-%02d", i)
	}
	time.Sleep(300 * time.Millisecond)
}
