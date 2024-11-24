// +build !debug

package main

import "fmt"

type StructDep struct {
	Field1 int
	Field2 string
}

func main() {
	fmt.Println("Release build")
	sd := StructDep{
		Field1: 42,
		Field2: "That's the truth",
	}
	fmt.Println(sd)
}
