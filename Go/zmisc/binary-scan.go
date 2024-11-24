package main

import (
	"bytes"
	"encoding/binary"
	"log"
)

func generateByteExample() *bytes.Buffer {
	sample := []int64{1, 2, 3, 4}
	buf := new(bytes.Buffer)
	for _, s := range sample {
		_ = binary.Write(buf, binary.LittleEndian, s)
	}
	return buf
}

type arrayint []int

func (arr *arrayint) Scan(x interface{}) error {
	src := x.([]byte)
	res := make([]int, len(src)/8)
	pos := 0
	for i := 0; i < len(src); i += 8 {
		b64 := bytes.NewReader(src[i : i+8])
		var val int64
		err := binary.Read(b64, binary.LittleEndian, &val)
		if err != nil {
			return err
		}
		res[pos] = int(val)
		pos++
	}
	*arr = res
	return nil
}

func main() {
	b := generateByteExample()
	samplebytes := b.Bytes()
	log.Println(samplebytes)
	res := arrayint{}
	if err := res.Scan(samplebytes); err != nil {
		log.Fatal(err)
	}
	log.Println(res)
}
