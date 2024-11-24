package main

import (
	"fmt"
	"math/rand"
)

type One struct{}
type Two struct{}
type Three struct{}
type Four struct{}
type Five struct{}

func main() {
	var a any = []any{One{}, Two{}, Three{}, Four{}, Five{}}[rand.Intn(5)]
	switch t := a.(type) {
	case One, Two, Three, Four, Five:
		_ = t
		fmt.Println("cachem'all")
	default:
		fmt.Println("unbreachable")
	}
}
