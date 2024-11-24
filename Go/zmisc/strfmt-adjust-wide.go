package main

import (
	"fmt"
	"strings"
)

func main() {
	max := 10
	for i := 1; i < max; i++ {
		for j := 0; j < i; j++ {
			stars := strings.Repeat("*", i-j)
			fmt.Printf("%*s", max-j, stars)
		}
		fmt.Println()
	}
}
