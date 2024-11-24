package main

import (
	"fmt"

	"github.com/mashingan/smapping"
)

func deflateExample() {
	type (
		nest1 struct {
			N1FieldInt int    `json:"nested1_int"`
			N1FieldStr string `json:"nested1_str"`
		}

		nest2 struct {
			N2FieldInt int    `json:"nested2_int"`
			N2FieldStr string `json:"nested2_str"`
		}

		outerobj struct {
			FieldInt int    `json:"field_int"`
			FieldStr string `json:"field_str"`
			Nest1    nest1  `json:"nested1"`
			Nest2    nest2  `json:"nested2"`
		}
	)

	src := outerobj{
		FieldInt: 5,
		FieldStr: "555",
		Nest1: nest1{
			N1FieldInt: 515,
			N1FieldStr: "515",
		},
		Nest2: nest2{
			N2FieldInt: 525,
			N2FieldStr: "525",
		},
	}
	srcmap := smapping.MapTagsFlatten(&src, "json")
	tgt := outerobj{}
	fmt.Println(smapping.FillStructDeflate(&tgt, srcmap, "json"))
	fmt.Printf("%#v\n", tgt.FieldInt)
	fmt.Printf("%#v\n", tgt.FieldStr)
	fmt.Printf("%#v\n", tgt.Nest1.N1FieldInt)
	fmt.Printf("%#v\n", tgt.Nest1.N1FieldStr)
	fmt.Printf("%#v\n", tgt.Nest2.N2FieldInt)
	fmt.Printf("%#v\n", tgt.Nest2.N2FieldStr)

	// Output:
	// <nil>
	// 5
	// "555"
	// 515
	// "515"
	// 525
	// "525"

}
