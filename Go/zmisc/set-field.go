package main

import (
	"fmt"
	"reflect"
)

type Mytype struct {
	Kawaii bool
	Nice   bool
	Spell  string
}

func setfield(obj interface{}, name string, value interface{}) {
	sval := reflect.ValueOf(obj).Elem()
	sfval := sval.FieldByName(name)
	val := reflect.ValueOf(value)
	sfval.Set(val)
}

func fillMytype(mytype interface{}, objmap map[string]interface{}) {
	for k, v := range objmap {
		setfield(mytype, k, v)
	}
}

func main() {
	fmt.Println("Hello, playground")
	mytype := Mytype{}
	fillMytype(&mytype, map[string]interface{}{
		"Kawaii": true,
		"Nice":   true,
		"Spell":  "Est",
	})
	fmt.Println(mytype)
}
