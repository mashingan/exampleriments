package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"strconv"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	fmt.Println("Masukkan Jumalah Elemen :")
	scanner.Scan()
	generateSlice, err := strconv.Atoi(scanner.Text())
	if err != nil {
		fmt.Println(err)
	}

	slice := generateSlice
	fmt.Println("Sebelum Sorting :", slice)
	bubblesort(slice)
	fmt.Println("Setelah Sorting :", slice)
}

func generateSlice(size int) []int {

	slice := make([]int, size, size)
	for i := 0; i < size; i++ {
		slice[i] = rand.Intn(10-1) + 1
	}
	return slice
}

func bubblesort(items []int) {
	var (
		n      = len(items)
		sorted = false
	)
	for !sorted {
		swapped := false
		for i := 0; i < n-1; i++ {
			if items[i] > items[i+1] {
				items[i+1], items[i] = items[i], items[i+1]
				swapped = true
			}
		}
		if !swapped {
			sorted = true
		}
		n = n - 1
	}
}
