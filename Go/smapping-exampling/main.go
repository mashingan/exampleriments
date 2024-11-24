package main

//lint:file-ignore U1000 no need to check

import (
	//"encoding/json"

	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/mashingan/smapping"
)

//lint:ignore U1000 used
type source struct {
	Label   string `json:"label"`
	Info    string `json:"info"`
	Version int    `json:"version"`
}

//lint:ignore U1000 used
type sink struct {
	Label string
	Info  string
}

type ptrString struct {
	Name *string `json:"name,omitempty" form:"name"`
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	// fmt.Println("vim-go")
	src := source{
		Label:   "source",
		Info:    "the origin",
		Version: 1,
	}
	mapped := smapping.MapFields(&src)
	fmt.Println("source:", mapped)
	dst := sink{}
	if err := smapping.FillStruct(&dst, mapped); err != nil {
		panic(err)
	}
	fmt.Println("sink:", dst)

	// arrobj()
	// arrint()

	// mapcfg()

	// fillFields()

	// TestMap_nested("json", "top", "level1", "level2", "ref_level3", "finally")
	// TestMap_nested("", "TopLayer", "Level1", "Level2", "RefLevel3", "What")
	// FillStructNestedTest(true)
	// FillStructNestedTest(false)

	// ExampleMapEncoder()
	// ExampleMapDecoder()
	// examplePtrPointer()
	deflateExample1()
	deflateExample2()
}

type (
	embedObj struct {
		FieldInt   int     `json:"fieldInt"`
		FieldStr   string  `json:"fieldStr"`
		FieldFloat float64 `json:"fieldFloat"`
	}
	// embedEmbed struct {
	// 	Embed1 embedObj  `json:"embed1"`
	// 	Embed2 *embedObj `json:"embed2"`
	// }
	embedObjs struct {
		Objs []*embedObj `json:"embeds"`
	}
)

//lint:ignore U1000 used
func arrobj() {
	// obj := embedEmbed{
	// 	Embed1: embedObj{1, "one", 1.1},
	// }
	objsem := embedObjs{
		Objs: []*embedObj{
			{1, "one", 1.1},
			{2, "two", 2.2},
			nil,
			{4, "four", 3.3},
			{5, "five", 4.4},
		},
	}
	maptag := smapping.MapTags(&objsem, "json")
	embedstf, ok := maptag["embeds"].([]interface{})
	// embedstf, ok := maptag["embeds"].([]*embedObj)
	if !ok {
		log.Fatalf("Wrong type, %#v, %T", maptag["embeds"], maptag["embeds"])
	}
	for _, emtf := range embedstf {
		emb := emtf
		log.Printf("%#v\n", emb)
	}

	rawtfobj := smapping.Mapped{
		"embeds": []smapping.Mapped{
			{"fieldInt": 1, "fieldStr": "one", "fieldFloat": 1.1},
			{"fieldInt": 2, "fieldStr": "two", "fieldFloat": 2.2},
			nil,
			{"fieldInt": 4, "fieldStr": "four", "fieldFloat": 4.4},
			{"fieldInt": 5, "fieldStr": "five", "fieldFloat": 5.5},
		},
		// "embeds": []*embedObj{
		// 	{1, "one", 1.1},
		// 	{2, "two", 2.2},
		// 	{4, "four", 4.4},
		// 	{5, "five", 5.5},
		// },
	}
	// var newemb embedObjs
	newemb := embedObjs{
		// Objs: []*embedObj{{}, {}, {}, {}, {}},
		// Objs: []*embedObj{},
	}
	err := smapping.FillStructByTags(&newemb, rawtfobj, "json")
	if err != nil {
		log.Println(err)
	}
	log.Printf("%#v\n", newemb)
	for _, ob := range newemb.Objs {
		log.Printf("%#v\n", ob)
	}
}

//lint:ignore U1000 used
func arrint() {
	type (
		ArrInt   []int
		MyArrInt struct {
			ArrInt `json:"array_int"`
		}
		APint []*int
		MPint struct {
			APint `json:"ptarr_int"`
		}
		APfloat []*float32
		MPfloat struct {
			APfloat `json:"ptarr_float"`
		}
	)

	initobj := MyArrInt{
		ArrInt: []int{1, 2, 3, 4, 5},
	}
	minitobj := smapping.MapTags(&initobj, "json")
	// arintminit, ok := minitobj["array_int"].(ArrInt)
	arintminit, ok := minitobj["array_int"].([]interface{})
	if !ok {
		log.Println("failed to cast")
		return
	}
	log.Printf("arrintminit %#v\n", arintminit)
	rawminit := smapping.Mapped{
		"array_int": []int{5, 4, 3, 2, 1},
	}
	var rinit MyArrInt
	if err := smapping.FillStructByTags(&rinit, rawminit, "json"); err != nil {
		log.Println(err)
	}
	log.Printf("rinit %#v\n", rinit)

	a := new(int)
	b := new(int)
	c := new(int)
	d := new(int)
	e := new(int)
	*a = 11
	*b = 22
	*c = 33
	*d = 44
	*e = 55
	pinitobj := MPint{
		APint: []*int{a, b, nil, c, d, e},
	}
	mapinit := smapping.MapTags(&pinitobj, "json")
	// rawpinit, ok := mapinit["ptarr_int"].(APint)
	rawpinit, ok := mapinit["ptarr_int"].([]interface{})
	if !ok {
		log.Println("failed conv")
		return
	}
	// log.Printf("rawpinit %v\n", rawpinit)
	for _, rp := range rawpinit {
		newrp, ok := rp.(int)
		if !ok {
			log.Printf("failed to write rp %#v\n", rp)
			continue
		}
		log.Printf("rp %#v\n", newrp)
	}

	rawpinit2 := smapping.Mapped{
		"ptarr_int": []interface{}{55, 44, 33, nil, 22, 11},
	}
	var pinit2 MPint
	if err := smapping.FillStructByTags(&pinit2, rawpinit2, "json"); err != nil {
		log.Println(err)
	}
	for _, p := range pinit2.APint {
		if p == nil {
			log.Printf("*r nil %#v\n", p)
		} else {
			log.Printf("*r %d %#v\n", *p, p)
		}
	}

	rawfloat := smapping.Mapped{
		"ptarr_float": []interface{}{1.1, 2.2, nil, 3.3, 4.4},
	}
	var mfloat MPfloat
	if err := smapping.FillStructByTags(&mfloat, rawfloat, "json"); err != nil {
		log.Println(err)
	}
	for _, p := range mfloat.APfloat {
		if p == nil {
			log.Printf("*r nil %#v\n", p)
		} else {
			log.Printf("*r %f %#v\n", *p, p)
		}
	}
}

//lint:ignore U1000 used
type config struct {
	Limit     int       `cfg:"LIMIT"`
	LogPrefix string    `cfg:"LOG_PREFIX"`
	ResetTime time.Time `cfg:"RESET_CLOCK"`
	Floaters  []float32 `cfg:"FLOAT_COUNTS"`
}

//lint:ignore U1000 used
type ConvertFunc func(interface{}) (interface{}, error)

//lint:ignore U1000 used
type MapConvert map[string]ConvertFunc

//lint:ignore U1000 used
func makeConverter(op func(s string) (interface{}, error)) ConvertFunc {
	return func(x interface{}) (interface{}, error) {
		str, ok := x.(string)
		if !ok {
			return nil, fmt.Errorf("invalid value provided, expect string got %T", x)
		}
		return op(str)
	}
}

//lint:ignore U1000 used
func mapcfg() {
	os.Setenv("LIMIT", "10")
	os.Setenv("LOG_PREFIX", "INFO")
	os.Setenv("RESET_CLOCK", "23:59:59")
	os.Setenv("FLOAT_COUNTS", "1.1,2.2,3.3,4.4,5.5")

	cfgmap, _ := mapConvert(&config{}, "cfg", MapConvert{
		"LIMIT": makeConverter(func(numstr string) (interface{}, error) {
			num, err := strconv.Atoi(numstr)
			if err != nil {
				return nil, err
			}
			return num, nil
		}),
		"RESET_CLOCK": makeConverter(func(clockstr string) (interface{}, error) {
			t, err := time.Parse("15:04:05", clockstr)
			if err != nil {
				return nil, err
			}
			return t, nil
		}),
		"FLOAT_COUNTS": makeConverter(func(floats string) (interface{}, error) {
			flts := strings.Split(floats, ",")
			res := make([]float32, len(flts))
			for i, flt := range flts {
				tempval, err := strconv.ParseFloat(flt, 32)
				if err != nil {
					log.Println(err)
					continue
				}
				res[i] = float32(tempval)
			}
			return res, nil
		}),
	})
	cfgobj := config{}
	if err := smapping.FillStructByTags(&cfgobj, cfgmap, "cfg"); err != nil {
		log.Println(err)
	}
	log.Printf("%v\n", cfgobj)
}

//lint:ignore U1000 used
func mapConvert(x interface{}, tag string, mapc MapConvert) (smapping.Mapped, error) {
	xmap := smapping.MapTags(x, tag)
	for k := range xmap {
		valenv := os.Getenv(k)
		if valenv == "" {
			continue
		}
		if f, ok := mapc[k]; ok {
			val, err := f(valenv)
			if err != nil {
				// return nil, err
				log.Println(err)
				continue
			}
			xmap[k] = val
		} else {
			xmap[k] = valenv
		}
	}
	return xmap, nil
}

func examplePtrPointer() {
	mapstr := map[string]interface{}{
		"name": "hello world",
	}
	p := ptrString{}
	log.Println(smapping.FillStructByTags(&p, mapstr, "json"))
	log.Printf("%#v\n", p)
	log.Printf("%#v\n", *p.Name)
	s := "hello異世界"
	log.Println(ptrString{
		Name: &s,
	})
	mapstr["name"] = nil
	p = ptrString{}
	log.Println(smapping.FillStructByTags(&p, mapstr, "json"))
	log.Printf("%#v\n", p)
	log.Printf("%#v\n", p.Name)
	// log.Printf("%#v\n", *p.Name)
}

func deflateExample1() {
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

func deflateExample2() {
	type (
		nest2 struct {
			N2FieldInt int    `json:"nested2_int"`
			N2FieldStr string `json:"nested2_str"`
		}

		nest1 struct {
			N1FieldInt int    `json:"nested1_int"`
			N1FieldStr string `json:"nested1_str"`
			Nest2      nest2  `json:"nested2"`
		}

		outerobj struct {
			FieldInt int    `json:"field_int"`
			FieldStr string `json:"field_str"`
			Nest1    nest1  `json:"nested1"`
		}
	)

	src := outerobj{
		FieldInt: 5,
		FieldStr: "555",
		Nest1: nest1{
			N1FieldInt: 515,
			N1FieldStr: "515",
			Nest2: nest2{
				N2FieldInt: 525,
				N2FieldStr: "525",
			},
		},
	}
	srcmap := smapping.MapTagsFlatten(&src, "json")
	tgt := outerobj{}
	fmt.Println(smapping.FillStructDeflate(&tgt, srcmap, "json"))
	fmt.Printf("%#v\n", tgt.FieldInt)
	fmt.Printf("%#v\n", tgt.FieldStr)
	fmt.Printf("%#v\n", tgt.Nest1.N1FieldInt)
	fmt.Printf("%#v\n", tgt.Nest1.N1FieldStr)
	fmt.Printf("%#v\n", tgt.Nest1.Nest2.N2FieldInt)
	fmt.Printf("%#v\n", tgt.Nest1.Nest2.N2FieldStr)

	// Output:
	// <nil>
	// 5
	// "555"
	// 515
	// "515"
	// 525
	// "525"

}
