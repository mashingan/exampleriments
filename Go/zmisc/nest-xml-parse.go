// golang playground:
// https://go.dev/play/p/MXXp9W_6m4h
package main

import (
	"bytes"
	"encoding/xml"
	"fmt"
	"strings"
)

const raw = `
<outer>
	<field1>val1</field1>
	<field2>val2</field2>
	<tags>
		<tag1>v1</tag1>
		<tag2>v2</tag2>
		<tag3>v3</tag3>
		<tag4>v4</tag4>
	</tags>
	<tags>
		<tag1>v1</tag1>
		<tag2>v2</tag2>
		<tag3>v3</tag3>
		<tag4>v4</tag4>
	</tags>
</outer>
`

func main() {
	decoder := xml.NewDecoder(bytes.NewBufferString(raw))
	tolkiens := []xml.StartElement{}
	indent := 0
	for {
		token, err := decoder.Token()
		if err != nil {
			fmt.Println("\nerr:", err)
			break
		}
		switch telm := token.(type) {
		case xml.StartElement:
			if len(tolkiens) > 0 {
				fmt.Println()
				indent += 2
			}
			tolkiens = append(tolkiens, telm)
			fmt.Printf("%s%s: ", strings.Repeat(" ", indent), telm.Name.Local)
		case xml.EndElement:
			tolkiens = tolkiens[:len(tolkiens)-1]
			indent -= 2
		case xml.CharData:
			fmt.Print(strings.TrimSpace(string(telm)))
		}
	}
}
