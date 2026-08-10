package main

import (
	"fmt"
	"unsafe"
)

func main() {
	byteSlice := []byte{1, 2, 3, 4, 5}

	// Method 1: Using copy
	var array1 [5]byte
	copy(array1[:], byteSlice)
	fmt.Println("Array using copy:", array1)

	// Method 2: Using unsafe
	array2 := *(*[5]byte)(unsafe.Pointer(&byteSlice[0]))
	fmt.Println("Array using unsafe:", array2)
}
