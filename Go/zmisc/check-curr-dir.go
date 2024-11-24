package main

import (
	"fmt"
	"os"
)

func main() {
	p, err := os.Getwd()
	if err != nil {
		panic(err)
	}
	fmt.Println(p)
}
