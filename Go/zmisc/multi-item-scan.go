package main

import (
	"bytes"
	"fmt"
)

func main() {
	fmt.Println("Hello, playground")
	ints := make([]int, 3)
	rawinputs := []byte(`1 2 3 4 5`)
	n, err := fmt.Scan(&ints[0], &ints[1], &ints[2])
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println("n: ", n)
	fmt.Println(ints)

	ints2 := make([]int, 3)
	n, err = fmt.Fscan(bytes.NewReader(rawinputs), &ints2[0], &ints2[1], &ints2[2])
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println("n: ", n)
	fmt.Println(ints2)
}
