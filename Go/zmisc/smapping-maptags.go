package main

import (
	"fmt"
	"log"
	"time"

	"github.com/mashingan/smapping"
)

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
	maptags := smapping.MapTags(&source, "json")
	log.Println("maptags:", maptags)
	hereticsink := HereticSink{}
	err := smapping.FillStructByTags(&hereticsink, maptags, "json")
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
}

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

type HereticSink struct {
	NahLabel  string      `json:"label"`
	HahaInfo  string      `json:"info"`
	Version   string      `json:"heretic_version"`
	Now       time.Time   `json:"when"`
	Intervals []int       `json:"intervals"`
	Embed     []*embedObj `json:"embed"`
}
