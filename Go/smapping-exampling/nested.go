package main

//lint:file-ignore U1000 no need to check

import (
	"log"

	"github.com/mashingan/smapping"
)

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

func TestMap_nested(tag string, fields ...string) {
	madnestMap := smapping.MapTags(&madnestStruct, tag)
	if len(madnestMap) != 1 {
		log.Printf("Got empty smapping.Mapped, expected 1")
		return
	}
	count := 0
	log.Printf("madnestMap: %#v\n", madnestMap)
	top, ok := madnestMap[fields[count]]
	if !ok {
		log.Printf("Failed to get top field")
		return
	}
	count++
	lv1, ok := top.(smapping.Mapped)[fields[count]]
	if !ok {
		log.Printf("Failed to get level 1 field")
		return
	}
	count++
	lv2, ok := lv1.(smapping.Mapped)[fields[count]]
	if !ok {
		log.Printf("Failed to get level 2 field")
		return
	}
	count++
	reflv3, ok := lv2.(smapping.Mapped)[fields[count]]
	if !ok {
		log.Printf("Failed to get ref level 3 field")
		return
	}
	count++
	what, ok := reflv3.(smapping.Mapped)[fields[count]]
	if !ok {
		log.Printf("Failed to get the inner ref level 3")
		return
	}
	switch v := what.(type) {
	case string:
		theval := what.(string)
		if theval != "matryoska" {
			log.Printf("Expected matryoska, got %s", theval)
		}
	default:
		log.Printf("Expected string, got %T", v)
	}
}

func FillStructNestedTest(bytag bool) {
	var madnestObj MadNest
	var err error
	if bytag {
		madnestMap := smapping.MapTags(&madnestStruct, "json")
		log.Printf("madnestMap: %#v\n", madnestMap)
		err = smapping.FillStructByTags(&madnestObj, madnestMap, "json")
	} else {
		madnestMap := smapping.MapFields(&madnestStruct)
		log.Printf("madnestMap: %#v\n", madnestMap)
		err = smapping.FillStruct(&madnestObj, madnestMap)
	}
	if err != nil {
		log.Printf("%s", err.Error())
		return
	}
	log.Printf("madnestObj %#v\n", madnestObj)
	log.Printf("madnestObj..Level2.RefLevel3 %#v\n", madnestObj.Level1.Level2.RefLevel3.What)
	if madnestObj.TopLayer.Level1.Level2.RefLevel3.What != "matryoska" {
		log.Printf("Error: expected \"matroska\" got \"%s\"", madnestObj.Level1.Level2.RefLevel3.What)
	}
}
