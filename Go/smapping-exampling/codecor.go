package main

//lint:file-ignore U1000 no need to check
import (
	"fmt"

	"github.com/mashingan/smapping"
)

type sometype struct {
	Value string `json:"value"`
}

func (s sometype) MapEncode() (interface{}, error) {
	return s.Value, nil
}

func (s *sometype) MapDecode(x interface{}) error {
	if x == nil {
		s.Value = ""
		return nil
	}
	xstr, ok := x.(string)
	if !ok {
		return nil
	}
	s.Value = xstr
	return nil
}

func ExampleMapEncoder() {
	type MapEncoderExample struct {
		Data sometype `json:"data"`
	}

	s := MapEncoderExample{Data: sometype{Value: "hello"}}
	smap := smapping.MapTags(&s, "json")
	str, ok := smap["data"].(string)
	if !ok {
		fmt.Println("Not ok!")
		return
	}
	fmt.Println(str)

	// Output:
	// hello
}

func ExampleMapDecoder() {
	type MapDecoderExample struct {
		Data sometype `json:"data"`
	}

	smap := map[string]interface{}{
		"data": "hello",
	}
	var s MapDecoderExample
	err := smapping.FillStructByTags(&s, smap, "json")
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println("data:", s.Data.Value)

	// Output:
	// data: hello
}
