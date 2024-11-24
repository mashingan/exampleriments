package main

import (
	"encoding/json"
	"fmt"
)

type nullOrEmpty string

func (s *nullOrEmpty) UnmarshalJSON(sbyte []byte) error {
	temps := string(sbyte)
	if temps == "" || temps == "null" {
		*s = nullOrEmpty("")

	} else {
		*s = nullOrEmpty(temps)
	}
	return nil
}

type response struct {
	Data nullOrEmpty `json:"data,omitempty"`
}

type res2 struct {
	Data string `json:"data,omitempty"`
}

func main() {
	raw1 := []byte(`{"data": "test test"}`)
	raw2 := []byte(`{}`)
	raw3 := []byte(`{"data": null}`)
	var s response
	err := json.Unmarshal(raw1, &s)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println("raw 1:", s)
	s = response{}
	err = json.Unmarshal(raw2, &s)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println("raw 2:", s)
	err = json.Unmarshal(raw3, &s)
	s = response{}
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println("raw 3:", s)
	r := res2{}
	fmt.Println("raw 3:", r)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println("raw 3:", r)
}
