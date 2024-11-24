package main

import (
	"encoding/json"
	"fmt"

	"github.com/mashingan/smapping"
)

type (
	nest2 struct {
		N2FieldInt float64 `json:"nested2_int"`
		N2FieldStr string  `json:"nested2_str"`
	}

	nest1 struct {
		N1FieldInt float64 `json:"nested1_int"`
		N1FieldStr string  `json:"nested1_str"`
		Nest2      *nest2  `json:"nested2"`
	}

	outerobj struct {
		FieldInt float64 `json:"field_int"`
		FieldStr string  `json:"field_str"`
		Nest1    nest1   `json:"nested1"`
	}
)

func main() {
	rawb := `
{
	"field_str": "5",
	"field_int": 5,
	"nested1_int": 515,
	"nested1_str": "515",
	"nested2_int": 525,
	"nested2_str": "525"
}`
	var m map[string]interface{}
	fmt.Println(json.Unmarshal([]byte(rawb), &m))
	fmt.Println(m)
	for k, v := range m {
		fmt.Printf("%s: %#v %T\n", k, v, v)
	}
	var tgt outerobj
	fmt.Println(smapping.FillStructDeflate(&tgt, m, "json"))
	fmt.Printf("%#v\n", tgt)
	fmt.Printf("%#v\n", tgt.Nest1.Nest2)

	src := outerobj{
		FieldInt: 5,
		FieldStr: "555",
		Nest1: nest1{
			N1FieldInt: 515,
			N1FieldStr: "515",
			Nest2: &nest2{
				N2FieldInt: 525,
				N2FieldStr: "525",
			},
		},
	}
	srcmap := smapping.MapTagsFlatten(&src, "json")
	tgt2 := outerobj{}
	fmt.Println("srcmap:", srcmap)
	fmt.Println(smapping.FillStructDeflate(&tgt2, srcmap, "json"))
	fmt.Printf("%#v\n", tgt2.FieldInt)
	fmt.Printf("%#v\n", tgt2.FieldStr)
	fmt.Printf("%#v\n", tgt2.Nest1.N1FieldInt)
	fmt.Printf("%#v\n", tgt2.Nest1.N1FieldStr)
	fmt.Printf("%#v\n", tgt2.Nest1.Nest2.N2FieldInt)
	fmt.Printf("%#v\n", tgt2.Nest1.Nest2.N2FieldStr)
}
