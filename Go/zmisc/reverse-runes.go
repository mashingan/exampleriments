package main

import (
	"bytes"
	"fmt"
)

func main() {
	h := "hello 異世界"
	fmt.Println(h)
	runes := bytes.Runes([]byte(h))
	fmt.Println(runes)
	runlen := len(runes)
	for i := 0; i < runlen/2; i++ {
		temp := runes[i]
		runes[i] = runes[runlen-1-i]
		runes[runlen-1-i] = temp
	}
	fmt.Println(runes)
	fmt.Println(string(runes))
}
