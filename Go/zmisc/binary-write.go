// example ref:
//https://golang.org/pkg/encoding/binary/#example_Write
package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
)

func main() {
	buf := new(bytes.Buffer)
	var data = []interface{}{
		uint16(61374),
		int8(-54),
		uint8(254),
	}
	for _, v := range data {
		err := binary.Write(buf, binary.LittleEndian, v)
		if err != nil {
			fmt.Println("Writing binary failed:", v)
		}
	}
	fmt.Printf("%x\n", buf.Bytes())
}
