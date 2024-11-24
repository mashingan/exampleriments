package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println("start")
	defer fmt.Println("end")
	time.Sleep(3 * time.Minute)
}
