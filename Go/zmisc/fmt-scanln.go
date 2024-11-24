package main

import (
	"fmt"
)

func main() {
	line := make([]string, 10)
	toinput := make([]interface{}, len(line))
	for i := range line {
		toinput[i] = &line[i]
	}

	fmt.Print("Masukan baris:")
	strnum, _ := fmt.Scanln(toinput...)
	fmt.Println("Yang dimasukkan:", line[:strnum])
}
