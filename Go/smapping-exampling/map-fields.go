package main

//lint:file-ignore U1000 no need to check

import (
	"log"

	"github.com/mashingan/smapping"
)

//lint:ignore U1000 used
func fillFields() {
	initobjs := embedObjs{
		Objs: []*embedObj{
			{1, "one", 1.1},
			{2, "two", 2.2},
			{4, "four", 4.4},
			{5, "five", 5.5},
		},
	}
	rawtfobj := smapping.Mapped{
		"Objs": []smapping.Mapped{
			{"FieldInt": 1, "FieldStr": "one", "FieldFloat": 1.1},
			{"FieldInt": 2, "FieldStr": "two", "FieldFloat": 2.2},
			nil,
			{"FieldInt": 4, "FieldStr": "four", "FieldFloat": 4.4},
			{"FieldInt": 5, "FieldStr": "five", "FieldFloat": 5.5},
		},
	}
	initmap := smapping.MapFields(&initobjs)
	log.Printf("%#v\n", initmap)
	var objembs embedObjs
	if err := smapping.FillStruct(&objembs, rawtfobj); err != nil {
		log.Println(err)
	}
	for _, o := range objembs.Objs {
		log.Printf("%#v\n", o)
	}
}
