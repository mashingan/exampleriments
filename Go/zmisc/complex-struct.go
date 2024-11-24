package main

import (
	"encoding/json"
	"fmt"
)

type Incoming struct {
	Type    string `json:"type"`
	Details []Detail
}

type Detail struct {
	Quantity int `json:"quantity"`
}

func main() {
	raw := []byte(`{"type": "this", "this": [{"quantity": 1}]}`)
	res := Incoming{}
	err := json.Unmarshal(raw, &res)
	if err != nil {
		fmt.Println(err)
		return
	}
	placeholder := map[string]interface{}{}
	_ = json.Unmarshal(raw, &placeholder)
	if err != nil {
		fmt.Println(err)
		return
	}
	for _, d := range placeholder[res.Type].([]interface{}) {
		rawdet, err := json.Marshal(d.(map[string]interface{}))
		if err != nil {
			continue
		}
		dst := Detail{}
		err = json.Unmarshal(rawdet, &dst)
		if err != nil {
			fmt.Println(err)
			continue
		}
		res.Details = append(res.Details, dst)
	}

	fmt.Println(placeholder)
	fmt.Println(res)
}
