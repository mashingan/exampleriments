package main

import (
	"fmt"
	"time"
)

func main() {
block:
	for {
		select {
		case <-time.After(1 * time.Second):
			fmt.Println("break loop")
			break block
		}
	}
}
