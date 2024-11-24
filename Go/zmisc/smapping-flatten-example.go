package main

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/mashingan/smapping"
)

type (
	Object struct {
		*Outer
	}
	Outer struct {
		*Inner1
	}
	Inner1 struct {
		*Inner2
	}
	Inner2 struct {
		*Inner3
	}
	Inner3 struct {
		*Inner4
	}
	Inner4 struct {
		*Last
	}
	Last struct{}
)

var rawjson = []byte(`
{
	"Outer": {
		"Inner1": {
			"Inner2": {
				"Inner3": null
			}
		}
	}
}`)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	var obj Outer
	if err := json.Unmarshal(rawjson, &obj); err != nil {
		log.Fatal(err)
	}
	m := smapping.MapTagsFlatten(&obj, "")
	for k, v := range m {
		log.Printf("key: %v -> value: %v\n", k, v)
	}
	fmt.Println("vim-go")
}
