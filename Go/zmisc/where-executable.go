package main

import (
	"os"
	"fmt"
)

func main() {
	where, _ := os.Executable()
	fmt.Println("where does it run from?", where)
}
