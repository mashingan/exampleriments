package main

import (
	"log"
	"time"

	"github.com/mashingan/smapping"
)

type collapseObj struct {
	Obj1 embedObject
	Obj2 embedObject
	Obj3 embedObject
}

type embedObject struct {
	Field1 string    `json:"field1str"`
	Field2 int       `json:"field2int"`
	Field3 bool      `json:"field3bool"`
	Field4 time.Time `json:"nao"`
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	nao := time.Now()
	source := collapseObj{
		Obj1: embedObject{
			Field1: "1 field",
			Field2: 142,
			Field3: true,
			Field4: nao,
		},
		Obj2: embedObject{
			Field2: 242,
			Field4: nao,
		},
		Obj3: embedObject{
			Field1: "3 field",
			Field2: 342,
		},
	}
	smap := smapping.MapTagsFlatten(&source, "json")
	log.Println("smap:", smap)
	var coll collapseObj
	err := smapping.FillStructByTags(&coll, smap, "json")
	if err != nil {
		log.Println("err fill:", err)
	}
	log.Println("coll:", coll)
	collmap := smapping.Mapped{}
	err = smapping.FillStructByTags(&collmap, smap, "json")
	if err != nil {
		log.Println("err fill map:", err)
	}
	log.Println("collmap:", collmap)
}
