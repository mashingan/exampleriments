package main

import "fmt"

func main() {
	drawXYZ(5)
}

func drawXYZ(N int) {
	n := 1
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			if n%3 == 0 {
				fmt.Print("X")
			} else if n%2 == 0 {
				fmt.Print("Z")
			} else {
				fmt.Print("Y")
			}
			fmt.Print(" ")
			n += 1
		}
		fmt.Println()
	}
}
