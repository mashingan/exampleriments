package main

import (
	"fmt"
	"runtime"
	"time"

	"github.com/mashingan/smapping"
)

type test struct {
	FieldInt int       `json api bson:"fieldInt"`
	FieldStr string    `json api bson:"fieldStr"`
	Nao      time.Time `json api bson:"nao"`
	Truthy   bool      `json api bson:"truthy"`
}

func main() {
	fmt.Println("Hello, playground")
	fmt.Println("version:", runtime.Version())
	nao := time.Now()
	t := test{
		FieldInt: 42,
		FieldStr: "test",
		Nao:      nao,
		Truthy:   true,
	}
	fmt.Println("t:", t)
	tmap := smapping.MapTags(&t, "api")
	fmt.Println("tmap:", tmap)

	tfill := test{}
	if err := smapping.FillStructByTags(&tfill, tmap, "bson"); err != nil {
		fmt.Println("tfill err:", err)
	}
	fmt.Println("tfill:", tfill)

}
