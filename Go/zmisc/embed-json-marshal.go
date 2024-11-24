package main

import (
	"encoding/json"
	"fmt"
)

type somecfg struct {
	Val   string `json:"val"`
	Label string `json:"label"`
}


type Response[Data any] struct {
	*somecfg
	Data  Data   `json:"data,omitempty"`
}

func main() {
	cfg1 := somecfg{"1", "one"}
	r := Response[struct{}]{somecfg: &cfg1}
	b, err := json.Marshal(r)
	fmt.Println("err:", err)
	fmt.Println("bod:", string(b))
}
