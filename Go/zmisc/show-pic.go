package main

import (
	"fmt"
	"golang.org/x/tour/pic"
)

func Pic(dx, dy int) [][]uint8 {
	res := make([][]uint8, dx)
	for x := 0; x < dx; x++ {
		res[x] = make([]uint8, dy)
	}
	for x := 0; x < dx; x++ {
		for y := 0; y < dy; y++ {
			res[x][y] = uint8(x+y) / uint8(2)
		}
	}
	return res
}

func main() {
	fmt.Println("vim-go")
	pic.Show(Pic)
}
