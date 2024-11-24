package main

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

func toggleVowelShift(sin string) (sout string) {
	const (
		vowel = "aeiou"
		volen = len(vowel)
	)
	sinlower := strings.ToLower(sin)
	for i, c := range sinlower {
		runval, _ := utf8.DecodeRuneInString(sin[i:])
		cap := unicode.IsUpper(runval)
		idx := strings.IndexRune(vowel, unicode.ToLower(runval))
		if idx > -1 {
			c = rune(vowel[(idx+1)%volen])
		}
		if !cap {
			c = unicode.ToUpper(c)
		}
		sout += fmt.Sprintf("%c", c)

	}
	return
}

func main() {
	fmt.Println(toggleVowelShift("heHEHAhahuehue"))
	fmt.Println(toggleVowelShift("Hello異世界"))
	fmt.Println(toggleVowelShift("qwertyuiopQWERTYUIOP"))
}
