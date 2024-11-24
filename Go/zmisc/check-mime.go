package main

import (
	"fmt"
	"mime"
)

func main() {
	fmt.Println(mime.TypeByExtension(".xlsx"))
	fmt.Println(mime.TypeByExtension(".xls"))
}
