package main

import (
	"fmt"

	lib "gitlab.smartfren.com/paggr/libraries"
)

func main() {
	amt := lib.Amount{
		Currency: "RP",
		Value:    5000,
	}
	fmt.Println(amt)
}
