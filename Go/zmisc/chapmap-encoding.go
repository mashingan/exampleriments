package main

import (
	"bytes"
	"log"

	"golang.org/x/text/encoding/charmap"
)

const strExample = "Slatkevičiusa"

func main() {
	log.Println(strExample)
	buff := new(bytes.Buffer)
	for _, utf8char := range strExample {
		r := rune(utf8char)
		b, ok := charmap.ISO8859_1.EncodeRune(r)
		if ok {
			buff.WriteRune(r)
		} else {
			buff.WriteByte(b)
		}
	}
	log.Println(buff.String())

}
