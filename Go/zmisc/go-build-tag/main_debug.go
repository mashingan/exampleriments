// +build debug

// ref: https://stackoverflow.com/a/38951389
// to build a debug, do
// go build --tags debug
package main

import "fmt"

type StructDep struct {
	Field1    int
	Field2    string
	DebugData string
	IsDebug   bool
}

func main() {
	fmt.Println("Debug build")
	sd := StructDep{
		Field1:    42,
		Field2:    "That's the truth",
		DebugData: "This is debug build",
		IsDebug:   true,
	}
	fmt.Println(sd)
}
