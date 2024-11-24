package main

import (
	"fmt"
	"reflect"
)

type MyType struct {
	TypeA string
	TypeB float64
	TypeC bool
}

func fillMyType(obj interface{}, vals ...interface{}) {
	sval := reflect.ValueOf(obj).Elem()
	for i, v := range vals {
		val := reflect.ValueOf(v)
		sfval := sval.Field(i)
		sfval.Set(val)
	}
}

func main() {
	mytype := MyType{}
	fillMyType(&mytype, "Est", 42.0, true)
	fmt.Println(mytype)
}
