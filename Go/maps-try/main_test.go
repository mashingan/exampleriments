package main

import (
	"testing"
	"time"

	"github.com/mashingan/smapping"
	"github.com/mitchellh/mapstructure"
)

type source struct {
	Label    string    `json:"label"     mapstructure:"label"`
	Info     string    `json:"info"      mapstructure:"info"`
	Version  int       `json:"version"   mapstructure:"version"`
	Toki     time.Time `json:"tomare"    mapstructure:"tomare"`
	Fuwafuwa float32   `json:"fuwafuwa"  mapstructure:"fuwafuwa"`
	Addr     *string   `json:"address"   mapstructure:"address"`
}

var toki = time.Date(2000, time.January, 1, 0, 0, 0, 0, time.UTC)
var hello = "hello異世界"

var sourceobj source = source{
	Label:    "source",
	Info:     "the origin",
	Version:  1,
	Toki:     toki,
	Addr:     &hello,
	Fuwafuwa: 0.32,
}

func BenchmarkFillStruct(b *testing.B) {
	m := smapping.MapFields(sourceobj)
	var s source
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		smapping.FillStruct(&s, m)
	}
	b.Logf("s: %#v\n", s)

}

func BenchmarkMapStructure(b *testing.B) {
	m := smapping.MapTags(sourceobj, "mapstructure")
	var s source
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		mapstructure.Decode(m, &s)
	}
	b.Logf("s: %#v\n", s)
}

func BenchmarkFillStructByTags(b *testing.B) {
	m := smapping.MapTags(sourceobj, "json")
	var s source
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		smapping.FillStructByTags(&s, m, "json")
	}
	b.Logf("s: %#v\n", s)
}
