package main

import "fmt"

func main() {
	var x, y int
	fmt.Print("masukkan x: ")
	fmt.Scanf("%d\n", &x)
	fmt.Print("masukkan y: ")
	fmt.Scanf("%d\n", &y)
	fmt.Printf("ini x: %d\nini y: %d\n", x, y)
}
