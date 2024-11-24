package main

import (
	"encoding/json"
	"fmt"
	"log"
	"time"

	"github.com/mashingan/smapping"
)

type embedObj struct {
	FieldInt   int     `json:"fieldInt"`
	FieldStr   string  `json:"fieldStr"`
	FieldFloat float64 `json:"fieldFloat"`
}

type Source struct {
	Label     string      `json:"label"`
	Info      string      `json:"info"`
	Version   int         `json:"version"`
	Now       time.Time   `json:"when"`
	Intervals []int       `json:"intervals"`
	Embed     []*embedObj `json:"embed"`
}

type Sink struct {
	Label     string
	Info      string
	Now       time.Time   `json:"when"`
	Intervals []int       `json:"intervals"`
	Embed     []*embedObj `json:"embed"`
}

type HereticSink struct {
	NahLabel  string      `json:"label"`
	HahaInfo  string      `json:"info"`
	Version   string      `json:"heretic_version"`
	Now       time.Time   `json:"when"`
	Intervals []int       `json:"intervals"`
	Embed     []*embedObj `json:"embed"`
}

type DifferentOneField struct {
	Name    string `json:"name"`
	Label   string `json:"label"`
	Code    string `json:"code"`
	Private string `json:"private" api:"internal"`
}

type differentSourceSink struct {
	Source Source      `json:"source"`
	Sink   HereticSink `json:"heretic_sink"`
}

func ExampleMapTags_nested() {
	var sourceobj = Source{
		Label:   "source",
		Info:    "the origin",
		Version: 1,
	}
	nestedSource := differentSourceSink{
		Source: sourceobj,
		Sink: HereticSink{
			NahLabel: "nested diff",
			HahaInfo: "nested info",
			Version:  "next version",
		},
	}
	nestedMap := smapping.MapTags(&nestedSource, "json")
	log.Println(nestedSource)
	log.Println(nestedMap)
	// Output:
	// map
}

type RefLevel3 struct {
	What string `json:"finally"`
}
type Level2 struct {
	*RefLevel3 `json:"ref_level3"`
}
type Level1 struct {
	Level2 `json:"level2"`
}
type TopLayer struct {
	Level1 `json:"level1"`
}
type MadNest struct {
	TopLayer `json:"top"`
}

var madnestStruct MadNest = MadNest{
	TopLayer: TopLayer{
		Level1: Level1{
			Level2: Level2{
				RefLevel3: &RefLevel3{
					What: "matryoska",
				},
			},
		},
	},
}

func main() {
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	source := Source{
		Label:     "source",
		Info:      "the origin",
		Version:   1,
		Now:       time.Now(),
		Intervals: []int{1, 2, 3, 4, 5},
		Embed: []*embedObj{
			&embedObj{1, "one", 1.1},
			&embedObj{2, "two", 2.2},
			//&embedObj{3, "three", 3.3},
			nil,
			&embedObj{4, "four", 4.4},
			&embedObj{5, "five", 5.5},
		},
	}
	log.Println("source:", source)
	mapped := smapping.MapFields(&source)
	log.Println("mapped:", mapped)
	for k, v := range mapped {
		log.Println("key:", k)
		fmt.Printf("val: %v type %T\n", v, v)
	}
	source2 := &Source{
		Label:   "source",
		Info:    "the origin",
		Version: 1,
	}
	log.Println("source2:", source2)
	mapped = smapping.MapFields(source2)
	log.Println("mapped:", mapped)
	sink := Sink{}
	err := smapping.FillStruct(&sink, mapped)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("sink:", sink)

	maptags := smapping.MapTags(&source, "json")
	log.Println("maptags:", maptags)
	hereticsink := HereticSink{}
	err = smapping.FillStructByTags(&hereticsink, maptags, "json")
	if err != nil {
		log.Fatal(err)
	}
	log.Println("heretic sink:", hereticsink)
	for i, em := range hereticsink.Embed {
		if em == nil {
			continue
		}
		fmt.Printf("heretic embed: %d, %v\n", i, *em)
	}

	ExampleMapTags_nested()
	log.Println("=============")
	recvjson := []byte(`{"name": "bella", "label": "balle", "code": "albel", "private": "allbe"}`)
	dof := DifferentOneField{}
	_ = json.Unmarshal(recvjson, &dof)
	log.Println("unmarshaled struct:", dof)

	marshaljson, _ := json.Marshal(dof)
	log.Println("marshal back:", string(marshaljson))

	// What we want actually "internal" instead of "private" field
	// we use the api tags on to make the json
	apijson, _ := json.Marshal(smapping.MapTagsWithDefault(&dof, "api", "json"))
	log.Println("api marshal:", string(apijson))

	log.Println("=============")
	// This time is the reverse, we receive "internal" field when
	// we need to receive "private" field to match our json tag field
	respjson := []byte(`{"name": "bella", "label": "balle", "code": "albel", "internal": "allbe"}`)
	respdof := DifferentOneField{}
	_ = json.Unmarshal(respjson, &respdof)
	log.Println("unmarshal resp:", respdof)

	// to get that, we should put convert the json to Mapped first
	jsonmapped := smapping.Mapped{}
	_ = json.Unmarshal(respjson, &jsonmapped)
	// now we fill our struct respdof
	_ = smapping.FillStructByTags(&respdof, jsonmapped, "api")
	log.Println("full resp:", respdof)
	returnback, _ := json.Marshal(respdof)
	log.Println("marshal resp back:", string(returnback))
	// first we unmarshal respdof, we didn't get the "private" field
	// but after our mapping, we get "internal" field value and
	// simply marshaling back to `returnback`

	TestFillStructByTags_nested()
	TestFillStruct_nested()
	log.Println("=============")
	testTime()
	log.Println("=============")
	testPtrTime()
	log.Println("=============")
	ExampleMapTagsFlatten()
	log.Println("=============")
	testNilValue()
}

func TestFillStructByTags_nested() {
	madnestMap := smapping.MapTags(&madnestStruct, "json")
	log.Println("madnestMap:", madnestMap)
	var madnestObj MadNest
	err := smapping.FillStructByTags(&madnestObj, madnestMap, "json")
	if err != nil {
		fmt.Printf("%s", err.Error())
		return
	}
	log.Println("madnestObj:", madnestObj)
	if madnestObj.TopLayer.Level1.Level2.RefLevel3.What != "matryoska" {
		fmt.Printf("Error: expected \"matroska\" got \"%s\"", madnestObj.Level1.Level2.RefLevel3.What)
	}
	fmt.Printf("What value: %s\n", madnestObj.TopLayer.Level1.Level2.RefLevel3.What)
}

func TestFillStruct_nested() {
	madnestMap := smapping.MapFields(&madnestStruct)
	log.Println("madnestMap:", madnestMap)
	var madnestObj MadNest
	err := smapping.FillStruct(&madnestObj, madnestMap)
	if err != nil {
		fmt.Printf("%s", err.Error())
		return
	}
	log.Println("madnestObj:", madnestObj)
	if madnestObj.TopLayer.Level1.Level2.RefLevel3.What != "matryoska" {
		fmt.Printf("Error: expected \"matroska\" got \"%s\"", madnestObj.Level1.Level2.RefLevel3.What)
	}
	fmt.Printf("What value: %s\n", madnestObj.TopLayer.Level1.Level2.RefLevel3.What)
}

func testTime() {
	type timeMap struct {
		Label string    `json:"label"`
		Time  time.Time `json:"definedTime"`
	}
	now := time.Now()
	obj := timeMap{Label: "test", Time: now}
	{
		jsbyte, err := json.Marshal(obj)
		if err != nil {
			log.Fatal(err)
		}
		log.Println(string(jsbyte))
		mapp := smapping.Mapped{}
		err = json.Unmarshal(jsbyte, &mapp)
		if err != nil {
			log.Fatal(err)
		}
		log.Println(mapp)
		objTarget := timeMap{}
		err = smapping.FillStructByTags(&objTarget, mapp, "json")
		if err != nil {
			log.Fatal(err)
		}
		log.Println(obj)
		log.Println(objTarget)
		log.Println(objTarget.Time.Equal(obj.Time))
	}
	{
		mapfield := smapping.MapFields(&obj)
		log.Println(mapfield)
		jsbyte, err := json.Marshal(mapfield)
		if err != nil {
			log.Fatal(err)
		}
		log.Println("jsbyte2:", string(jsbyte))
		objTarget := timeMap{}
		mapp := smapping.Mapped{}
		err = json.Unmarshal(jsbyte, &mapp)
		if err != nil {
			log.Fatal(err)
		}
		err = smapping.FillStruct(&objTarget, mapp)
		if err != nil {
			log.Fatal(err)
		}
		log.Println(objTarget)
	}

}

func testPtrTime() {
	type timeMap struct {
		Label   string     `json:"label"`
		PtrTime *time.Time `json:"ptrTime"`
	}
	now := time.Now()
	obj := timeMap{Label: "test", PtrTime: &now}
	{
		jsbyte, err := json.Marshal(obj)
		if err != nil {
			log.Fatal(err)
		}
		log.Println(string(jsbyte))
		mapp := smapping.Mapped{}
		err = json.Unmarshal(jsbyte, &mapp)
		if err != nil {
			log.Fatal("unmarshal jsbyte: " + err.Error())
		}
		log.Println(mapp)
		objTarget := timeMap{}
		err = smapping.FillStructByTags(&objTarget, mapp, "json")
		if err != nil {
			log.Fatal("fill struct by tags: " + err.Error())
			log.Fatal(err)
		}
		log.Println(obj)
		log.Println(objTarget)
		log.Println(objTarget.PtrTime.Equal(*obj.PtrTime))
	}
	{
		mapfield := smapping.MapFields(&obj)
		log.Println(mapfield)
		jsbyte, err := json.Marshal(mapfield)
		if err != nil {
			log.Fatal(err)
		}
		log.Println("jsbyte2:", string(jsbyte))
		objTarget := timeMap{}
		mapp := smapping.Mapped{}
		err = json.Unmarshal(jsbyte, &mapp)
		if err != nil {
			log.Fatal(err)
		}
		err = smapping.FillStruct(&objTarget, mapp)
		if err != nil {
			log.Fatal(err)
		}
		log.Println(objTarget)
	}

}

type (
	Lv1 struct {
		Lv2
		Lv1Str string `json:"lv1str"`
	}
	Lv2 struct {
		Lv2Str string `json:"lv2str"`
		Lv3
	}
	Lv3 struct {
		Lv3Str string `json:"lv3str"`
		*Last
	}
	Last struct {
		Final string `json:"finally"`
	}
)

func ExampleMapTagsFlatten() {
	obj := Lv1{
		Lv1Str: "level 1 string",
		Lv2: Lv2{
			Lv2Str: "level 2 string",
			Lv3: Lv3{
				Lv3Str: "level 3 string",
				Last: &Last{
					Final: "Destination",
				},
			},
		},
	}
	nests := smapping.MapTagsFlatten(&obj, "json")
	for k, v := range nests {
		fmt.Printf("key: %s, value: %v\n", k, v)
	}
	js, err := json.Marshal(nests)
	if err != nil {
		log.Fatal(err)
	}
	log.Println(string(js))
	// Unordered Output:
	// key: finally, value: matryoska
}

func testNilValue() {
	type embedEmbed struct {
		Embed1 embedObj  `json:"embed1"`
		Embed2 *embedObj `json:"embed2"`
	}
	//&embedObj{1, "one", 1.1},
	obj := embedEmbed{
		Embed1: embedObj{1, "one", 1.1},
	}
	log.Println("obj:", obj)
	objmap := smapping.MapTags(&obj, "json")
	log.Println("objmap:", objmap)
	embed2 := embedEmbed{}
	if err := smapping.FillStructByTags(&embed2, objmap, "json"); err != nil {
		log.Println("objmap fill fail:", err)
	}
	log.Println("embed2:", embed2)
	log.Println("embed2.Embed2:", embed2.Embed2)
	//=====//
	log.Println("fields map")
	objmap = smapping.MapFields(&obj)
	log.Println("objmap:", objmap)
	embed2 = embedEmbed{}
	if err := smapping.FillStruct(&embed2, objmap); err != nil {
		log.Println("objmap fields fill fail:", err)
	}
	log.Println("embed2:", embed2)
	log.Println("embed2.Embed2:", embed2.Embed2)
}
