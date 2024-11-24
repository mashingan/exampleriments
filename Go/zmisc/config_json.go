package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	f, err := os.OpenFile("config_json.json", os.O_RDONLY, 0644)
	if err != nil {
		panic(err)
	}
	defer f.Close()
	b, err := ioutil.ReadAll(f)
	if err != nil {
		panic(err)
	}
	var data []interface{}
	err = json.Unmarshal(b, &data)
	if err != nil {
		panic(err)
	}
	fmt.Println(data)

}
